{-# LANGUAGE OverloadedLists #-}

module API.Common where

import Control.Category ((.))
import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Lens (view, (.~), (?~))
import Control.Monad.Time (MonadTime (..))
import qualified Crypto.JWT as JWT
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  genericToJSON,
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.OpenApi (
  NamedSchema (..),
  OpenApiType (OpenApiObject),
  ToSchema (..),
  declareSchemaRef,
  genericDeclareNamedSchema,
  schemaName,
 )
import Data.OpenApi.Lens (properties, type_)
import Data.Pool (Pool)
import Database.Persist.Sql (runSqlPool, toSqlKey)
import Database.Persist.Sqlite (SqlBackend)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Model.Common (DBAction, aesonDropPrefixOptions, schemaDropPrefixOptions)
import Model.Entities (Key, User)
import qualified Network.HTTP.Types as HTTP
import Relude hiding (Set, (.))
import WebGear.Server

-- The API handlers run in the App monad.

newtype AppEnv = AppEnv {appEnvSqlBackend :: Pool SqlBackend}

newtype App a = App {unApp :: ReaderT AppEnv IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader AppEnv
    , MonadIO
    , MonadThrow
    , MonadCatch
    )

instance MonadTime App where
  currentTime = liftIO currentTime
  monotonicTime = liftIO monotonicTime

instance JWT.MonadRandom App where
  getRandomBytes = liftIO . JWT.getRandomBytes

askConnectionPool :: (MonadReader AppEnv m) => m (Pool SqlBackend)
askConnectionPool = asks appEnvSqlBackend

runDBAction :: DBAction a -> App a
runDBAction action = do
  pool <- askConnectionPool
  liftIO $ runSqlPool action pool

--------------------------------------------------------------------------------

okDescription :: Description
okDescription = "Successful operation"

dupDescription :: Text -> Description
dupDescription name = Description $ "Duplicate " <> name

resp403Description :: Description
resp403Description = "Not authenticated"

resp404Description :: Text -> Description
resp404Description name = Description $ name <> " not found"

withDoc :: (Handler h m) => Summary -> Description -> Middleware h ts ts
withDoc summ descr handler = handler . setDescription descr . setSummary summ

--------------------------------------------------------------------------------

type JSONBody a = Body JSON a

-- Middlewares for JWT authentication with "token" scheme

type AuthHeader = AuthorizationHeader "token"
type RequiredAuth = JWTAuth' Required "token" App () (Key User)
type OptionalAuth = JWTAuth' Optional "token" App () (Key User)

requiredTokenAuth ::
  ( StdHandler h App
  , Gets h [RequiredAuth, AuthHeader] Request
  , Sets h [RequiredResponseHeader "Content-Type" Text, JSONBody ErrorResponse] Response
  ) =>
  JWT.JWK ->
  Middleware h ts (RequiredAuth : AuthHeader : ts)
requiredTokenAuth jwk =
  optionalLenientHeader @"Authorization" @(AuthToken "token")
    . tokenAuth jwk (`jwtAuth'` forbidden)
  where
    forbidden = proc (_, err) -> case err of
      JWTAuthTokenBadFormat e ->
        setDescription "Authentication failure"
          . respondJsonA HTTP.forbidden403
          -<
            show @ErrorResponse e
      e ->
        setDescription "Unauthorized"
          . respondJsonA HTTP.unauthorized401
          -<
            show @ErrorResponse e

optionalTokenAuth ::
  ( StdHandler h App
  , Gets h [OptionalAuth, AuthHeader] Request
  ) =>
  JWT.JWK ->
  Middleware h ts (OptionalAuth : AuthHeader : ts)
optionalTokenAuth jwk =
  optionalLenientHeader @"Authorization" @(AuthToken "token")
    . tokenAuth jwk optionalJWTAuth'

tokenAuth ::
  ( Handler h App
  , Get h AuthHeader Request
  ) =>
  JWT.JWK ->
  (JWTAuth' x "token" App () (Key User) -> Middleware h ts (r : ts)) ->
  Middleware h ts (r : ts)
tokenAuth jwk auth nextHandler =
  auth authCfg (nextHandler . setDescription "JWT authorization based on `token' scheme")
  where
    authCfg =
      JWTAuth'
        { jwtValidationSettings = JWT.defaultJWTValidationSettings $ const True
        , jwkSet = JWT.JWKSet [jwk]
        , toJWTAttribute = claimsToUser
        }

    claimsToUser :: JWT.ClaimsSet -> App (Either () (Key User))
    claimsToUser claims = pure
      $ case view JWT.claimSub claims >>= readMaybe . toString . view JWT.string of
        Nothing -> Left ()
        Just oid -> Right $ toSqlKey oid

--------------------------------------------------------------------------------

-- A "wrapped" json body. Realworld API spec consumes and returns JSON
-- objects wrapped under a key in a top level object. The @Wrapped@
-- type encodes/decodes such objects.

newtype Wrapped (s :: Symbol) t = Wrapped {unwrap :: t}
  deriving newtype (IsString)

instance (KnownSymbol s, FromJSON t) => FromJSON (Wrapped s t) where
  parseJSON = withObject "json object" $ \obj ->
    Wrapped <$> obj .: fromString (symbolVal $ Proxy @s)

instance (KnownSymbol s, ToJSON t) => ToJSON (Wrapped s t) where
  toJSON (Wrapped x) = object [fromString (symbolVal $ Proxy @s) .= toJSON x]

instance (KnownSymbol s, ToSchema t) => ToSchema (Wrapped s t) where
  declareNamedSchema _ = do
    tSchema <- declareSchemaRef (Proxy @t)
    pure
      $ NamedSchema (("wrapped" <>) <$> schemaName (Proxy @t))
      $ mempty
      & type_
      ?~ OpenApiObject
        & properties
      .~ [(fromString (symbolVal $ Proxy @s), tSchema)]

type PathVarSlug = PathVar "slug" Text

respondJsonA ::
  ( StdHandler h m
  , Sets h [ResponseHeader Required "Content-Type" Text, JSONBody body] Response
  ) =>
  HTTP.Status ->
  h body Response
respondJsonA status = respondA status JSON

jsonRequestBody ::
  forall t ts h m.
  (StdHandler h m, Get h (JSONBody t) Request) =>
  h (Request `With` ts, Text) Response ->
  Middleware h ts (JSONBody t : ts)
jsonRequestBody = requestBody JSON

badRequestBody ::
  ( StdHandler h m
  , Sets h [RequiredResponseHeader "Content-Type" Text, JSONBody ErrorResponse] Response
  ) =>
  h a Response
badRequestBody = proc _ ->
  setDescription "Invalid request body"
    . respondJsonA HTTP.badRequest400
    -<
      "Could not parse body" :: ErrorResponse

badRequestParam ::
  ( StdHandler h m
  , Sets h [RequiredResponseHeader "Content-Type" Text, JSONBody ErrorResponse] Response
  ) =>
  h (Request `With` ts, ParamParseError) Response
badRequestParam = proc (_, e) ->
  setDescription "Invalid query parameter"
    . respondJsonA HTTP.badRequest400
    -<
      show @ErrorResponse e

--------------------------------------------------------------------------------

-- Error responses

newtype ErrorRecord = ErrorRecord
  {errorBody :: [Text]}
  deriving stock (Generic)

instance ToJSON ErrorRecord where
  toJSON = genericToJSON aesonDropPrefixOptions

instance ToSchema ErrorRecord where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

instance IsString ErrorRecord where
  fromString s = ErrorRecord{errorBody = [fromString s]}

type ErrorResponse = Wrapped "errors" ErrorRecord

type JSONBodyOrError a = [RequiredResponseHeader "Content-Type" Text, JSONBody a, JSONBody ErrorResponse]
