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
import Database.SQLite.Simple (NamedParam (..), Only (..), Query)
import Database.SQLite.Simple.QQ (sql)
import Model.Common
import Model.Entities
import Relude

data CreateUserPayload = CreateUserPayload
  { userUsername :: !Text
  , userEmail :: !Text
  , userPassword :: !Text
  }
  deriving stock (Generic)

data UserRecord = UserRecord
  { userId :: !Int64
  , userUsername :: !Text
  , userEmail :: !Text
  , userBio :: !(Maybe Text)
  , userImage :: !(Maybe Text)
  , userToken :: !Text
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
create jwk CreateUserPayload{userUsername, userEmail, userPassword} = do
  let stmt :: Query
      stmt =
        [sql|
          INSERT INTO user (id, username, email, password, bio, image)
          VALUES (NULL, :username, :email, :password, NULL, NULL)
          RETURNING id
        |]

      params :: [NamedParam]
      params =
        [ ":username" := userUsername
        , ":email" := userEmail
        , ":password" := hashUserPassword userPassword
        ]

  queryNamed stmt params >>= \case
    [Only key] -> do
      userToken <- generateJWT jwk key
      pure
        UserRecord
          { userId = coerce key
          , userUsername
          , userEmail
          , userToken
          , userBio = Nothing
          , userImage = Nothing
          }
    x -> fail $ "Expected a single key, but got " ++ show x

hashUserPassword :: Text -> Text
hashUserPassword = show . Hash.hashWith Hash.SHA256 . (encodeUtf8 :: Text -> ByteString)

generateJWT :: JWT.JWK -> UserId -> DBAction Text
generateJWT jwk (UserId uid) = do
  Right jwt <- mkJWT jwk ["sub" .= show @Text uid]
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
  { email :: !Text
  , password :: !Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToSchema)

checkCredentials :: JWT.JWK -> LoginUserPayload -> DBAction (Maybe UserRecord)
checkCredentials jwk LoginUserPayload{..} = do
  let q :: Query
      q = [sql| SELECT * FROM user WHERE email = :email AND password = :password |]

      params :: [NamedParam]
      params = [":email" := email, ":password" := hashUserPassword password]

  queryNamed q params >>= \case
    [User{..}] -> do
      userToken <- generateJWT jwk userId
      pure $ Just $ UserRecord{userId = coerce userId, ..}
    _x -> pure Nothing

--------------------------------------------------------------------------------

getByKey :: JWT.JWK -> UserId -> DBAction (Maybe UserRecord)
getByKey jwk key = do
  let q :: Query
      q = [sql| SELECT * FROM user WHERE id = :id |]

  queryNamed q [":id" := key] >>= \case
    [User{..}] -> do
      userToken <- generateJWT jwk key
      pure $ Just UserRecord{userId = coerce key, ..}
    _x -> pure Nothing

--------------------------------------------------------------------------------

data UpdateUserPayload = UpdateUserPayload
  { userUsername :: !(Maybe Text)
  , userEmail :: !(Maybe Text)
  , userPassword :: !(Maybe Text)
  , userBio :: !(Maybe (Maybe Text))
  , userImage :: !(Maybe (Maybe Text))
  }
  deriving stock (Generic)

instance FromJSON UpdateUserPayload where
  parseJSON = genericParseJSON aesonDropPrefixOptions

instance ToSchema UpdateUserPayload where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

update :: JWT.JWK -> UserId -> UpdateUserPayload -> DBAction (Maybe UserRecord)
update jwk key UpdateUserPayload{..} = do
  let updates :: [(String, NamedParam)]
      updates =
        catMaybes
          [ (\x -> ("username = :username", ":username" := x)) <$> userUsername
          , (\x -> ("email = :email", ":email" := x)) <$> userEmail
          , (\x -> ("password = :password", ":password" := hashUserPassword x)) <$> userPassword
          , (\x -> ("bio = :bio", ":bio" := x)) <$> userBio
          , (\x -> ("image = :image", ":image" := x)) <$> userImage
          ]

      stmt :: Query
      stmt =
        [sql| UPDATE user SET |]
          <> fromString (intercalate "," $ map fst updates)
          <> [sql| WHERE id = :id |]

      params :: [NamedParam]
      params = (":id" := key) : map snd updates

  executeNamed stmt params

  getByKey jwk key
