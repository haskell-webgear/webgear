module Model.Comment (
  CreateCommentPayload (..),
  CommentRecord (..),
  create,
  list,
  Model.Comment.delete,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Maybe (fromJust)
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import Database.Esqueleto.Experimental as E
import qualified Database.Persist.Sql as DB
import Model.Common
import Model.Entities
import qualified Model.Profile as Profile
import Relude hiding (on)

newtype CreateCommentPayload = CreateCommentPayload
  {commentBody :: Text}
  deriving stock (Generic)

instance FromJSON CreateCommentPayload where
  parseJSON = genericParseJSON aesonDropPrefixOptions

instance ToSchema CreateCommentPayload where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

data CommentRecord = CommentRecord
  { commentId :: Int64
  , commentCreatedAt :: UTCTime
  , commentUpdatedAt :: UTCTime
  , commentBody :: Text
  , commentAuthor :: Maybe Profile.Profile
  }
  deriving stock (Generic)

instance ToJSON CommentRecord where
  toJSON = genericToJSON aesonDropPrefixOptions

instance ToSchema CommentRecord where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

create :: Key User -> Text -> CreateCommentPayload -> DBAction (Maybe CommentRecord)
create commentAuthor slug CreateCommentPayload{..} =
  getArticleIdBySlug slug >>= traverse doCreate
  where
    doCreate :: Key Article -> DBAction CommentRecord
    doCreate commentArticle = do
      commentCreatedAt <- liftIO getCurrentTime
      let commentUpdatedAt = commentCreatedAt
      commentId <- DB.insert Comment{..}
      fromJust <$> getCommentRecord (Just commentAuthor) commentId

getArticleIdBySlug :: Text -> DBAction (Maybe (Key Article))
getArticleIdBySlug slug =
  fmap unValue . listToMaybe
    <$> ( select $ do
            article <- from $ table @Article
            where_ (article ^. ArticleSlug ==. val slug)
            pure $ article ^. ArticleId
        )

getCommentRecord :: Maybe (Key User) -> Key Comment -> DBAction (Maybe CommentRecord)
getCommentRecord maybeUserId commentId = DB.get commentId >>= traverse mkRecord
  where
    mkRecord :: Comment -> DBAction CommentRecord
    mkRecord Comment{..} = do
      authorProfile <- Profile.getOne maybeUserId commentAuthor
      pure
        CommentRecord
          { commentId = DB.fromSqlKey commentId
          , commentAuthor = authorProfile
          , ..
          }

--------------------------------------------------------------------------------

list :: Maybe (Key User) -> Text -> DBAction [CommentRecord]
list maybeCurrentUserId slug = do
  commentIds <- select $ do
    (article :& comment) <-
      from $
        table @Article
          `innerJoin` table @Comment
          `on` (\(article :& comment) -> article ^. ArticleId ==. comment ^. CommentArticle)
    where_ (article ^. ArticleSlug ==. val slug)
    pure $ comment ^. CommentId
  comments <- traverse (getCommentRecord maybeCurrentUserId . unValue) commentIds
  pure $ catMaybes comments

--------------------------------------------------------------------------------

delete :: Key User -> Text -> Key Comment -> DBAction ()
delete authorId slug commentId = E.delete $ do
  comment <- from $ table @Comment
  where_ (comment ^. CommentId ==. val commentId)
  where_ (comment ^. CommentAuthor ==. val authorId)
  where_ $
    exists $ do
      article <- from $ table @Article
      where_ (comment ^. CommentArticle ==. article ^. ArticleId)
      where_ (article ^. ArticleSlug ==. val slug)
