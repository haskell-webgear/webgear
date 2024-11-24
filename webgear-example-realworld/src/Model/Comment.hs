{-# LANGUAGE QuasiQuotes #-}

module Model.Comment (
  CreateCommentPayload (..),
  CommentRecord (..),
  create,
  list,
  Model.Comment.delete,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.OpenApi (ToSchema (..), genericDeclareNamedSchema)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import Database.SQLite.Simple (NamedParam (..), Only (..), Query)
import Database.SQLite.Simple.QQ (sql)
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
  { commentId :: !Int64
  , commentCreatedAt :: !UTCTime
  , commentUpdatedAt :: !UTCTime
  , commentBody :: !Text
  , commentAuthor :: !(Maybe Profile.Profile)
  }
  deriving stock (Generic)

instance ToJSON CommentRecord where
  toJSON = genericToJSON aesonDropPrefixOptions

instance ToSchema CommentRecord where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

create :: UserId -> Text -> CreateCommentPayload -> DBAction (Maybe CommentRecord)
create commentAuthor slug CreateCommentPayload{..} =
  getArticleIdBySlug slug >>= fmap join . traverse doCreate
  where
    doCreate :: ArticleId -> DBAction (Maybe CommentRecord)
    doCreate commentArticle = do
      now <- liftIO getCurrentTime
      let stmt :: Query
          stmt =
            [sql|
              INSERT INTO comment (id, created_at, updated_at, body, article, author)
              VALUES (NULL, :createdAt, :updatedAt, :body, :article, :author)
              RETURNING id
            |]

          params :: [NamedParam]
          params =
            [ ":createdAt" := now
            , ":updatedAt" := now
            , ":body" := commentBody
            , ":article" := commentArticle
            , ":author" := commentAuthor
            ]
      [Only commentId] <- queryNamed stmt params
      getCommentRecord (Just commentAuthor) commentId

getArticleIdBySlug :: Text -> DBAction (Maybe ArticleId)
getArticleIdBySlug slug = do
  articleIds <-
    queryNamed @(Only ArticleId)
      [sql| SELECT id FROM article WHERE slug = :slug |]
      [":slug" := slug]
  pure $ listToMaybe $ map fromOnly articleIds

getCommentRecord :: Maybe UserId -> CommentId -> DBAction (Maybe CommentRecord)
getCommentRecord maybeUserId cid = do
  let mkRecord :: Comment -> DBAction CommentRecord
      mkRecord Comment{..} = do
        authorProfile <- Profile.getOne maybeUserId author
        pure
          CommentRecord
            { commentId = coerce commentId
            , commentAuthor = authorProfile
            , commentCreatedAt = createdAt
            , commentUpdatedAt = updatedAt
            , commentBody = body
            }

  comment <- listToMaybe <$> queryNamed [sql| SELECT * FROM comment WHERE id = :id |] [":id" := cid]
  traverse mkRecord comment

--------------------------------------------------------------------------------

list :: Maybe UserId -> Text -> DBAction [CommentRecord]
list maybeCurrentUserId slug = do
  commentIds <- do
    let q :: Query
        q =
          [sql|
            SELECT
              c.id
            FROM
              article AS a JOIN
              comment AS c ON a.id = c.article
            WHERE
              a.slug = :slug
          |]
    coerce @[Only CommentId] @[CommentId] <$> queryNamed q [":slug" := slug]

  comments <- traverse (getCommentRecord maybeCurrentUserId) commentIds
  pure $ catMaybes comments

--------------------------------------------------------------------------------

delete :: UserId -> Text -> CommentId -> DBAction ()
delete authorId slug commentId = do
  executeNamed
    [sql|
      DELETE FROM comment AS c
      WHERE
        id = :commentId AND
        author = :authorId AND
        EXISTS (
          SELECT a.id FROM article AS a WHERE
          c.article = a.id AND
          a.slug = :slug
        )
    |]
    [":commentId" := commentId, ":authorId" := authorId, ":slug" := slug]
