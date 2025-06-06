{-# LANGUAGE OverloadedLists #-}

module API.Common where

import Control.Exception.Safe (MonadCatch, MonadThrow)
import Control.Lens (view, (.~), (?~))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, ReaderT (..), asks)
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
import Data.Function ((&))
import Data.OpenApi (
  NamedSchema (..),
  OpenApiType (OpenApiObject),
  ToSchema (..),
  declareSchemaRef,
  genericDeclareNamedSchema,
  schemaName,
 )
import Data.OpenApi.Lens (properties, type_)
import Data.Pool (Pool, withResource)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text, unpack)
import Database.SQLite.Simple (Connection)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Model.Common (DBAction, aesonDropPrefixOptions, schemaDropPrefixOptions)
import Model.Entities (UserId (..))
import qualified Network.HTTP.Types as HTTP
import Text.Read (readMaybe)
import WebGear.Server
import Prelude

-- The API handlers run in the App monad.

newtype AppEnv = AppEnv {appEnvSqlPool :: Pool Connection}

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

askConnectionPool :: (MonadReader AppEnv m) => m (Pool Connection)
askConnectionPool = asks appEnvSqlPool

runDBAction :: DBAction a -> App a
runDBAction action = do
  pool <- askConnectionPool
  liftIO $
    withResource pool $
      runReaderT action

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
withDoc summ descr handler = handler <<< setDescription descr <<< setSummary summ

--------------------------------------------------------------------------------

type JSONBody a = Body JSON a

-- Middlewares for JWT authentication with "token" scheme

type AuthHeader = AuthorizationHeader "token"
type RequiredAuth = JWTAuth' Required "token" App () UserId
type OptionalAuth = JWTAuth' Optional "token" App () UserId

requiredTokenAuth ::
  ( StdHandler h App
  , Gets h [RequiredAuth, AuthHeader]
  , Sets h [RequiredResponseHeader "Content-Type" Text, JSONBody ErrorResponse]
  ) =>
  JWT.JWK ->
  Middleware h ts (RequiredAuth : AuthHeader : ts)
requiredTokenAuth jwk =
  optionalLenientHeader @"Authorization" @(AuthToken "token")
    <<< tokenAuth jwk (`jwtAuth'` forbidden)
  where
    forbidden = proc (_, err) -> case err of
      JWTAuthTokenBadFormat e ->
        setDescription "Authentication failure" <<< respondJsonA HTTP.forbidden403 -< showError e
      e ->
        setDescription "Unauthorized" <<< respondJsonA HTTP.unauthorized401 -< showError e

optionalTokenAuth ::
  ( StdHandler h App
  , Gets h [OptionalAuth, AuthHeader]
  ) =>
  JWT.JWK ->
  Middleware h ts (OptionalAuth : AuthHeader : ts)
optionalTokenAuth jwk =
  optionalLenientHeader @"Authorization" @(AuthToken "token")
    . tokenAuth jwk optionalJWTAuth'

tokenAuth ::
  ( Handler h App
  , Get h AuthHeader
  ) =>
  JWT.JWK ->
  (JWTAuth' x "token" App () UserId -> Middleware h ts (r : ts)) ->
  Middleware h ts (r : ts)
tokenAuth jwk auth nextHandler =
  auth authCfg (nextHandler <<< setDescription "JWT authorization based on `token' scheme")
  where
    authCfg =
      JWTAuth'
        { jwtValidationSettings = JWT.defaultJWTValidationSettings $ const True
        , jwkSet = JWT.JWKSet [jwk]
        , toJWTAttribute = claimsToUser
        }

    claimsToUser :: JWT.ClaimsSet -> App (Either () UserId)
    claimsToUser claims = pure $
      case view JWT.claimSub claims >>= readMaybe . unpack . view JWT.string of
        Nothing -> Left ()
        Just oid -> Right (UserId oid)

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
    pure $
      NamedSchema (("wrapped" <>) <$> schemaName (Proxy @t)) $
        mempty
          & type_
            ?~ OpenApiObject
          & properties
            .~ [(fromString (symbolVal $ Proxy @s), tSchema)]

type PathVarSlug = PathVar "slug" Text

respondJsonA ::
  ( StdHandler h m
  , Sets h [ResponseHeader Required "Content-Type" Text, JSONBody body]
  ) =>
  HTTP.Status ->
  h body Response
respondJsonA status = respondA status JSON

jsonRequestBody ::
  forall t ts h m.
  (StdHandler h m, Get h (JSONBody t)) =>
  h (Request `With` ts, Text) Response ->
  Middleware h ts (JSONBody t : ts)
jsonRequestBody = requestBody JSON

badRequestBody ::
  (StdHandler h m, Set h (JSONBody ErrorResponse)) =>
  h a Response
badRequestBody = proc _ ->
  setDescription "Invalid request body"
    <<< respondJsonA HTTP.badRequest400
    -<
      "Could not parse body" :: ErrorResponse

badRequestParam ::
  (StdHandler h m, Set h (JSONBody ErrorResponse)) =>
  h (Request `With` ts, ParamParseError) Response
badRequestParam = proc (_, e) ->
  setDescription "Invalid query parameter" <<< respondJsonA HTTP.badRequest400 -< showError e

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

showError :: (Show a) => a -> ErrorResponse
showError = fromString . show

type JSONBodyOrError a = [JSONBody a, JSONBody ErrorResponse]

--------------------------------------------------------------------------------

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = \case
  Left _l -> Nothing
  Right r -> Just r
