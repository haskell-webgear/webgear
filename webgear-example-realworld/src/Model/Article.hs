{-# LANGUAGE QuasiQuotes #-}

module Model.Article (
  CreateArticlePayload (..),
  ArticleRecord (..),
  create,
  getArticleBySlug,
  getArticleIdAndAuthorBySlug,
  UpdateArticlePayload (..),
  Model.Article.update,
  Model.Article.delete,
  Limit (..),
  Offset (..),
  ArticleListInput (..),
  articleList,
  ArticleFeedInput (..),
  articleFeed,
  favorite,
  unfavorite,
) where

import Control.Exception.Safe (catch, throw)
import Control.Lens ((?~))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  Value (Number),
  genericParseJSON,
  genericToJSON,
 )
import Data.Char (isSpace)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.OpenApi (
  ToSchema (..),
  default_,
  genericDeclareNamedSchema,
  maximum_,
  minimum_,
  paramSchemaToSchema,
 )
import Data.OpenApi.Internal.Schema (plain)
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Word (Word64)
import Database.SQLite.Simple (
  Error (..),
  NamedParam (..),
  Only (..),
  Query,
  SQLError (..),
 )
import Database.SQLite.Simple.QQ (sql)
import GHC.Generics (Generic)
import Model.Common
import Model.Entities
import qualified Model.Profile as Profile
import qualified Network.URI.Encode as URIEncode
import System.Random (randomIO)
import Web.HttpApiData (FromHttpApiData)

data CreateArticlePayload = CreateArticlePayload
  { articleTitle :: !Text
  , articleDescription :: !Text
  , articleBody :: !Text
  , articleTagList :: !(Maybe [Text])
  }
  deriving stock (Generic)

instance FromJSON CreateArticlePayload where
  parseJSON = genericParseJSON aesonDropPrefixOptions

instance ToSchema CreateArticlePayload where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

data ArticleRecord = ArticleRecord
  { articleSlug :: !Text
  , articleTitle :: !Text
  , articleDescription :: !Text
  , articleBody :: !Text
  , articleTagList :: ![Text]
  , articleCreatedAt :: !UTCTime
  , articleUpdatedAt :: !UTCTime
  , articleFavorited :: !Bool
  , articleFavoritesCount :: !Int
  , articleAuthor :: !(Maybe Profile.Profile)
  }
  deriving stock (Generic)

instance ToJSON ArticleRecord where
  toJSON = genericToJSON aesonDropPrefixOptions

instance ToSchema ArticleRecord where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

create :: UserId -> CreateArticlePayload -> DBAction (Maybe ArticleRecord)
create userId CreateArticlePayload{..} = do
  articleCreatedAt <- liftIO getCurrentTime
  let articleUpdatedAt = articleCreatedAt
      tags = fromMaybe [] articleTagList
  articleSlug <- slugify articleTitle

  let stmt :: Query
      stmt =
        [sql|
          INSERT INTO article (id, slug, title, description, body, created_at, updated_at, author)
          VALUES (NULL, :slug, :title, :description, :body, :createdAt, :updatedAt, :author)
          RETURNING id
        |]

      params :: [NamedParam]
      params =
        [ ":slug" := articleSlug
        , ":title" := articleTitle
        , ":description" := articleDescription
        , ":body" := articleBody
        , ":createdAt" := articleCreatedAt
        , ":updatedAt" := articleUpdatedAt
        , ":author" := userId
        ]

  queryNamed stmt params >>= \case
    [Only articleId] -> do
      forM_ tags $ \tagName -> do
        [Only tagId] <-
          queryNamed @(Only TagId)
            [sql| INSERT INTO tag (id, name) VALUES (NULL, :tagName) ON CONFLICT DO UPDATE SET id = id RETURNING id |]
            [":tagName" := tagName]
        executeNamed
          [sql| INSERT INTO article_tag (tagid, articleid) VALUES (:tagId, :articleId) |]
          [":tagId" := tagId, ":articleId" := articleId]

      getArticleRecord (Just userId) articleId
    _x -> pure Nothing

slugify :: (MonadIO m) => Text -> m Text
slugify s = liftIO $ do
  num <- randomIO
  let suffix = pack $ "-" <> show (num :: Word64)
  pure $
    (<> suffix) $
      Text.take 255 $
        Text.filter URIEncode.isAllowed $
          Text.map (\c -> if isSpace c then '-' else c) $
            Text.toLower s

getArticleRecord :: Maybe UserId -> ArticleId -> DBAction (Maybe ArticleRecord)
getArticleRecord maybeUserId articleId = do
  queryNamed [sql| SELECT * FROM article WHERE id = :articleId |] [":articleId" := articleId] >>= \case
    [article] -> Just <$> mkRecord maybeUserId article
    _x -> pure Nothing

mkRecord :: Maybe UserId -> Article -> DBAction ArticleRecord
mkRecord maybeUserId Article{..} = do
  articleTagList <- do
    let q :: Query
        q =
          [sql|
            SELECT
              t.name
            FROM
              article_tag AS a_t JOIN
              tag AS t ON a_t.tagid = t.id
            WHERE
              a_t.articleid = :articleId
          |]

        params :: [NamedParam]
        params = [":articleId" := articleId]

    coerce @[Only Text] @[Text] <$> queryNamed q params

  favoritedUsers <- do
    let q :: Query
        q = [sql| SELECT userid FROM favorite WHERE articleid = :articleId |]

        params :: [NamedParam]
        params = [":articleId" := articleId]

    coerce @[Only UserId] @[UserId] <$> queryNamed q params

  let articleFavorited = maybe False (`elem` favoritedUsers) maybeUserId
  let articleFavoritesCount = length favoritedUsers
  authorProfile <- Profile.getOne maybeUserId articleAuthor

  pure $ ArticleRecord{articleAuthor = authorProfile, ..}

--------------------------------------------------------------------------------

getArticleBySlug :: Maybe UserId -> Text -> DBAction (Maybe ArticleRecord)
getArticleBySlug maybeUserId slug = do
  queryNamed [sql| SELECT * FROM article WHERE slug = :slug |] [":slug" := slug] >>= \case
    [article] -> Just <$> mkRecord maybeUserId article
    _x -> pure Nothing

--------------------------------------------------------------------------------

getArticleIdAndAuthorBySlug :: Text -> DBAction (Maybe (ArticleId, UserId))
getArticleIdAndAuthorBySlug slug = do
  let q :: Query
      q = [sql| SELECT id, author FROM article WHERE slug = :slug |]

      params :: [NamedParam]
      params = [":slug" := slug]

  listToMaybe <$> queryNamed q params

--------------------------------------------------------------------------------

data UpdateArticlePayload = UpdateArticlePayload
  { articleTitle :: !(Maybe Text)
  , articleDescription :: !(Maybe Text)
  , articleBody :: !(Maybe Text)
  }
  deriving stock (Generic)

instance FromJSON UpdateArticlePayload where
  parseJSON = genericParseJSON aesonDropPrefixOptions

instance ToSchema UpdateArticlePayload where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

update ::
  -- | author
  UserId ->
  ArticleId ->
  UpdateArticlePayload ->
  DBAction (Maybe ArticleRecord)
update authorId articleId UpdateArticlePayload{..} = do
  newSlug <- traverse slugify articleTitle
  now <- liftIO getCurrentTime

  let updates :: [(String, NamedParam)]
      updates =
        catMaybes
          [ (\x -> ("title = :articleTitle", ":articleTitle" := x)) <$> articleTitle
          , (\x -> ("description = :articleDescription", ":articleDescription" := x)) <$> articleDescription
          , (\x -> ("body = :articleBody", ":articleBody" := x)) <$> articleBody
          , (\x -> ("slug = :articleSlug", ":articleSlug" := x)) <$> newSlug
          , Just ("updated_at = :updatedAt", ":updatedAt" := now)
          ]

      stmt :: Query
      stmt =
        [sql| UPDATE article SET |]
          <> fromString (intercalate "," $ map fst updates)
          <> [sql| WHERE id = :articleId AND author = :articleAuthor |]

      params :: [NamedParam]
      params = map snd updates <> [":articleId" := articleId, ":articleAuthor" := authorId]

  executeNamed stmt params

  getArticleRecord (Just authorId) articleId

--------------------------------------------------------------------------------

delete :: UserId -> Text -> DBAction ()
delete userId slug = do
  [Only articleId] <-
    queryNamed @(Only ArticleId)
      [sql| DELETE FROM article WHERE slug = :slug AND author = :author RETURNING id |]
      [":slug" := slug, ":author" := userId]

  executeNamed [sql| DELETE FROM article_tag WHERE articleid = :id |] [":id" := articleId]
  executeNamed [sql| DELETE FROM comment WHERE article = :id |] [":id" := articleId]
  executeNamed [sql| DELETE FROM favorite WHERE articleid = :id |] [":id" := articleId]

--------------------------------------------------------------------------------
newtype Limit = Limit {getLimit :: Integer}
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Num, FromHttpApiData)

instance ToSchema Limit where
  declareNamedSchema _ =
    plain $
      paramSchemaToSchema (Proxy @Integer)
        & (minimum_ ?~ 0)
        & (maximum_ ?~ 1000)
        & (default_ ?~ Number 20)

newtype Offset = Offset {getOffset :: Integer}
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Num, FromHttpApiData)

instance ToSchema Offset where
  declareNamedSchema _ =
    plain $
      paramSchemaToSchema (Proxy @Integer)
        & (minimum_ ?~ 0)
        & (maximum_ ?~ 1000)
        & (default_ ?~ Number 0)

data ArticleListInput = ArticleListInput
  { maybeCurrentUserId :: !(Maybe UserId)
  , maybeTag :: !(Maybe Text)
  , maybeAuthorName :: !(Maybe Text)
  , maybeFavoritedBy :: !(Maybe Text)
  , listLimit :: !Limit
  , listOffset :: !Offset
  }

articleList :: ArticleListInput -> DBAction [ArticleRecord]
articleList ArticleListInput{..} = do
  let filterTag :: (Query, [NamedParam])
      filterTag =
        case maybeTag of
          Nothing -> ([sql| TRUE |], [])
          Just "" -> ([sql| TRUE |], [])
          Just aTag ->
            ( [sql|
                EXISTS (
                  SELECT
                    t.id
                  FROM
                    article_tag AS a_t JOIN
                    tag AS t ON a_t.tagid = t.id
                  WHERE
                    t.name = :tagName AND
                    a_t.articleid = a.id
                )
              |]
            , [":tagName" := aTag]
            )

      filterAuthor :: (Query, [NamedParam])
      filterAuthor =
        case maybeAuthorName of
          Nothing -> ([sql| TRUE |], [])
          Just "" -> ([sql| TRUE |], [])
          Just authorName ->
            ( [sql|
                EXISTS (
                  SELECT
                    *
                  FROM
                    user AS u
                  WHERE
                    a.author = u.id AND
                    u.username = :authorName
                )
              |]
            , [":authorName" := authorName]
            )

      filterFavorite :: (Query, [NamedParam])
      filterFavorite =
        case maybeFavoritedBy of
          Nothing -> ([sql| TRUE |], [])
          Just "" -> ([sql| TRUE |], [])
          Just favUserName ->
            ( [sql|
                EXISTS (
                  SELECT
                    *
                  FROM
                    favorite AS f JOIN
                    user AS u ON f.userid = u.id
                  WHERE
                    a.id = f.articleid AND
                    u.username = :favUserName
                )
              |]
            , [":favUserName" := favUserName]
            )

  let q :: Query
      q =
        [sql| SELECT a.id FROM article AS a WHERE |]
          <> fst filterTag
          <> [sql| AND |]
          <> fst filterAuthor
          <> [sql| AND |]
          <> fst filterFavorite
          <> [sql|
               ORDER BY a.updated_at DESC
               LIMIT :lim
               OFFSET :offset
             |]

      params :: [NamedParam]
      params =
        [ ":lim" := fromInteger @Int64 (getLimit listLimit)
        , ":offset" := fromInteger @Int64 (getOffset listOffset)
        ]
          <> snd filterTag
          <> snd filterAuthor
          <> snd filterFavorite

  articleIds <- coerce @[Only ArticleId] @[ArticleId] <$> queryNamed q params
  articles <- traverse (getArticleRecord maybeCurrentUserId) articleIds
  pure $ catMaybes articles

--------------------------------------------------------------------------------

data ArticleFeedInput = ArticleFeedInput
  { currentUserId :: !UserId
  , listLimit :: !Limit
  , listOffset :: !Offset
  }

articleFeed :: ArticleFeedInput -> DBAction [ArticleRecord]
articleFeed ArticleFeedInput{..} = do
  let q :: Query
      q =
        [sql|
          SELECT
            a.id
          FROM
            article AS a JOIN
            follow AS f ON f.followee = a.author
          WHERE
            f.follower = :currentUser
          LIMIT :lim
          OFFSET :offset
        |]

      params :: [NamedParam]
      params =
        [ ":currentUser" := currentUserId
        , ":lim" := fromInteger @Int64 (getLimit listLimit)
        , ":offset" := fromInteger @Int64 (getOffset listOffset)
        ]

  articleIds <- queryNamed q params
  articles <- traverse (getArticleRecord (Just currentUserId) . fromOnly) articleIds
  pure $ catMaybes articles

--------------------------------------------------------------------------------

favorite :: UserId -> Text -> DBAction (Maybe ArticleRecord)
favorite userId slug =
  getArticleIdAndAuthorBySlug slug >>= \case
    Nothing -> pure Nothing
    Just (articleId, _) -> do
      let handleDBError :: SQLError -> DBAction ()
          handleDBError e
            | sqlError e == ErrorConstraint = pure ()
            | otherwise = throw e

      let stmt :: Query
          stmt = [sql| INSERT INTO favorite (userid, articleid) VALUES (:userId, :articleId) |]

          params :: [NamedParam]
          params = [":userId" := userId, ":articleId" := articleId]

      executeNamed stmt params `catch` handleDBError
      getArticleRecord (Just userId) articleId

--------------------------------------------------------------------------------

unfavorite :: UserId -> Text -> DBAction (Maybe ArticleRecord)
unfavorite userId slug =
  getArticleIdAndAuthorBySlug slug >>= \case
    Nothing -> pure Nothing
    Just (articleId, _) -> do
      let stmt :: Query
          stmt = [sql| DELETE FROM favorite WHERE userid = :userId AND articleid = :articleId |]

          params :: [NamedParam]
          params = [":userId" := userId, ":articleId" := articleId]

      executeNamed stmt params
      getArticleRecord (Just userId) articleId
