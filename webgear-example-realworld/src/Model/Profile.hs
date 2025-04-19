{-# LANGUAGE QuasiQuotes #-}

module Model.Profile (
  Profile (..),
  getOne,
  getByName,
  follow,
  unfollow,
) where

import Control.Exception.Safe (catch, throw)
import Data.Aeson (ToJSON (..), genericToJSON)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Data.Text (Text)
import Database.SQLite.Simple (
  Error (ErrorConstraint),
  NamedParam (..),
  Query,
  SQLError (..),
 )
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.Types (Only (..))
import GHC.Generics (Generic)
import Model.Common
import Model.Entities

data Profile = Profile
  { userUsername :: !Text
  , userBio :: !(Maybe Text)
  , userImage :: !(Maybe Text)
  , userFollowing :: !Bool
  }
  deriving stock (Generic)

instance ToJSON Profile where
  toJSON = genericToJSON aesonDropPrefixOptions

instance ToSchema Profile where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

getOne ::
  -- | current user (if any)
  Maybe UserId ->
  -- | user to get profile of
  UserId ->
  DBAction (Maybe Profile)
getOne maybeFollowerKey followeeKey =
  findProfile
    maybeFollowerKey
    [sql| SELECT * FROM user WHERE id = :id |]
    [":id" := followeeKey]

findProfile ::
  Maybe UserId ->
  Query ->
  [NamedParam] ->
  DBAction (Maybe Profile)
findProfile maybeFollowerKey selector params = do
  maybeResult <- listToMaybe <$> queryNamed selector params
  traverse mkProfile maybeResult
  where
    mkProfile :: User -> DBAction Profile
    mkProfile User{..} = do
      maybeFollowing <- traverse (isFollowing userId) maybeFollowerKey
      pure Profile{userFollowing = fromMaybe False maybeFollowing, ..}

    isFollowing :: UserId -> UserId -> DBAction Bool
    isFollowing followeeKey followerKey = do
      let q :: Query
          q = [sql| SELECT COUNT(*) FROM follow WHERE follower = :follower AND followee = :followee |]

      followCount <- queryNamed q [":follower" := followerKey, ":followee" := followeeKey]
      pure $ followCount == [Only (1 :: Int)]

--------------------------------------------------------------------------------

getByName ::
  -- | current user (if any)
  Maybe UserId ->
  -- | username to get profile of
  Text ->
  DBAction (Maybe Profile)
getByName maybeFollowerKey username =
  findProfile
    maybeFollowerKey
    [sql|SELECT * FROM user WHERE username = :username |]
    [":username" := username]

--------------------------------------------------------------------------------

follow :: UserId -> Text -> DBAction (Maybe Profile)
follow followerKey followeeUsername =
  getUserIdByName followeeUsername >>= \case
    Nothing -> pure Nothing
    Just followeeKey -> do
      let handleDBError :: SQLError -> DBAction ()
          handleDBError e
            | sqlError e == ErrorConstraint = pure ()
            | otherwise = throw e

      let stmt :: Query
          stmt = [sql| INSERT INTO follow (follower, followee) VALUES (:follower, :followee) |]

          params :: [NamedParam]
          params = [":follower" := followerKey, ":followee" := followeeKey]

      executeNamed stmt params `catch` handleDBError
      getOne (Just followerKey) followeeKey

getUserIdByName :: Text -> DBAction (Maybe UserId)
getUserIdByName name = do
  let q :: Query
      q = [sql| SELECT id FROM user WHERE username = :username |]

      params :: [NamedParam]
      params = [":username" := name]

  queryNamed q params <&> \case
    [Only uid] -> Just uid
    _x -> Nothing

--------------------------------------------------------------------------------

unfollow :: UserId -> Text -> DBAction (Maybe Profile)
unfollow followerKey followeeUsername =
  getUserIdByName followeeUsername >>= \case
    Nothing -> pure Nothing
    Just followeeKey -> do
      let stmt :: Query
          stmt = [sql| DELETE FROM follow WHERE follower = :follower AND followee = :followee |]

          params :: [NamedParam]
          params = [":follower" := followerKey, ":followee" := followeeKey]

      executeNamed stmt params

      getOne (Just followerKey) followeeKey
