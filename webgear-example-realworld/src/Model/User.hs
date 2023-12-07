{-# LANGUAGE OverloadedLists #-}

module Model.User (
  CreateUserPayload (..),
  UserRecord (..),
  create,
  LoginUserPayload (..),
  checkCredentials,
  getByKey,
  UpdateUserPayload (..),
  Model.User.update,
) where

import Control.Monad.Except (throwError)
import qualified Crypto.Hash as Hash
import qualified Crypto.JWT as JWT
import Data.Aeson (
  FromJSON (..),
  Object,
  Result (..),
  ToJSON (..),
  Value (..),
  fromJSON,
  genericParseJSON,
  genericToJSON,
  (.=),
 )
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Database.Esqueleto.Experimental as E
import qualified Database.Persist.Sql as DB
import Model.Common
import Model.Entities
import Relude

data CreateUserPayload = CreateUserPayload
  { userUsername :: Text
  , userEmail :: Text
  , userPassword :: Text
  }
  deriving stock (Generic)

data UserRecord = UserRecord
  { userId :: Int64
  , userUsername :: Text
  , userEmail :: Text
  , userBio :: Maybe Text
  , userImage :: Maybe Text
  , userToken :: Text
  }
  deriving stock (Generic)

instance FromJSON CreateUserPayload where
  parseJSON = genericParseJSON aesonDropPrefixOptions

instance ToSchema CreateUserPayload where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

instance ToJSON UserRecord where
  toJSON = genericToJSON aesonDropPrefixOptions

instance ToSchema UserRecord where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

create :: JWT.JWK -> CreateUserPayload -> DBAction UserRecord
create jwk CreateUserPayload{..} = do
  let userBio = Nothing
      userImage = Nothing
  key <- DB.insert User{userPassword = hashUserPassword userPassword, ..}
  userToken <- generateJWT jwk key
  pure UserRecord{userId = DB.fromSqlKey key, ..}

hashUserPassword :: Text -> Text
hashUserPassword = show . Hash.hashWith Hash.SHA256 . (encodeUtf8 :: Text -> ByteString)

generateJWT :: JWT.JWK -> Key User -> DBAction Text
generateJWT jwk uid = do
  Right jwt <- mkJWT jwk ["sub" .= show @Text (DB.fromSqlKey uid)]
  pure $ decodeUtf8 $ toStrict $ JWT.encodeCompact jwt

mkJWT ::
  JWT.JWK ->
  -- | claim set as a JSON object
  Object ->
  DBAction (Either JWT.JWTError JWT.SignedJWT)
mkJWT jwk claims = lift $ JWT.runJOSE $ do
  alg <- JWT.bestJWSAlg jwk
  let hdr = JWT.newJWSHeader ((), alg)
  case fromJSON (Object claims) of
    Error s -> throwError $ JWT.JWTClaimsSetDecodeError s
    Success claims' -> JWT.signClaims jwk hdr claims'

--------------------------------------------------------------------------------

data LoginUserPayload = LoginUserPayload
  { email :: Text
  , password :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToSchema)

checkCredentials :: JWT.JWK -> LoginUserPayload -> DBAction (Maybe UserRecord)
checkCredentials jwk LoginUserPayload{..} = do
  users <- select $ do
    u <- from $ table @User
    where_ (u ^. UserEmail ==. val email)
    where_ (u ^. UserPassword ==. val (hashUserPassword password))
    pure u
  case users of
    [Entity key User{..}] -> do
      userToken <- generateJWT jwk key
      pure $ Just $ UserRecord{userId = DB.fromSqlKey key, ..}
    _ -> pure Nothing

--------------------------------------------------------------------------------

getByKey :: JWT.JWK -> Key User -> DBAction (Maybe UserRecord)
getByKey jwk key = DB.get key >>= traverse mkRecord
  where
    mkRecord User{..} = do
      userToken <- generateJWT jwk key
      pure UserRecord{userId = DB.fromSqlKey key, ..}

--------------------------------------------------------------------------------

data UpdateUserPayload = UpdateUserPayload
  { userUsername :: Maybe Text
  , userEmail :: Maybe Text
  , userPassword :: Maybe Text
  , userBio :: Maybe (Maybe Text)
  , userImage :: Maybe (Maybe Text)
  }
  deriving stock (Generic)

instance FromJSON UpdateUserPayload where
  parseJSON = genericParseJSON aesonDropPrefixOptions

instance ToSchema UpdateUserPayload where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

update :: JWT.JWK -> Key User -> UpdateUserPayload -> DBAction (Maybe UserRecord)
update jwk key UpdateUserPayload{..} = do
  let updates =
        catMaybes
          [ UserUsername =?. userUsername
          , UserEmail =?. userEmail
          , UserPassword =?. (hashUserPassword <$> userPassword)
          , UserBio =?. userBio
          , UserImage =?. userImage
          ]
  E.update $ \u -> do
    set u updates
    where_ (u ^. UserId ==. val key)
  getByKey jwk key
