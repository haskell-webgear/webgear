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
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  Value (Number),
  genericParseJSON,
  genericToJSON,
 )
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Data.OpenApi (ToSchema (..), default_, genericDeclareNamedSchema, maximum_, minimum_, paramSchemaToSchema)
import Data.OpenApi.Internal.Schema (plain)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import Database.Esqueleto.Experimental as E
import qualified Database.Persist.Sql as DB
import Database.Sqlite (Error (..), SqliteException (..))
import Model.Common
import Model.Entities
import qualified Model.Profile as Profile
import qualified Network.URI.Encode as URIEncode
import Relude hiding (on)
import System.Random (randomIO)
import Web.HttpApiData (FromHttpApiData)

data CreateArticlePayload = CreateArticlePayload
  { articleTitle :: Text
  , articleDescription :: Text
  , articleBody :: Text
  , articleTagList :: Maybe [Text]
  }
  deriving stock (Generic)

instance FromJSON CreateArticlePayload where
  parseJSON = genericParseJSON aesonDropPrefixOptions

instance ToSchema CreateArticlePayload where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

data ArticleRecord = ArticleRecord
  { articleSlug :: Text
  , articleTitle :: Text
  , articleDescription :: Text
  , articleBody :: Text
  , articleTagList :: [Text]
  , articleCreatedAt :: UTCTime
  , articleUpdatedAt :: UTCTime
  , articleFavorited :: Bool
  , articleFavoritesCount :: Int
  , articleAuthor :: Maybe Profile.Profile
  }
  deriving stock (Generic)

instance ToJSON ArticleRecord where
  toJSON = genericToJSON aesonDropPrefixOptions

instance ToSchema ArticleRecord where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

create :: Key User -> CreateArticlePayload -> DBAction ArticleRecord
create userId CreateArticlePayload{..} = do
  articleCreatedAt <- liftIO getCurrentTime
  let articleUpdatedAt = articleCreatedAt
      articleAuthor = userId
      tags = fromMaybe [] articleTagList
  articleSlug <- slugify articleTitle

  articleId <- DB.insert Article{..}
  forM_ tags $ \tag -> do
    tagId <- DB.entityKey <$> DB.upsert (Tag tag) []
    DB.insert $ ArticleTag tagId articleId

  fromJust <$> getArticleRecord (Just userId) articleId

slugify :: MonadIO m => Text -> m Text
slugify s = liftIO $ do
  num <- randomIO
  let suffix = "-" <> show (num :: Word64)
  pure $
    (<> suffix) $
      Text.take 255 $
        Text.filter URIEncode.isAllowed $
          Text.map (\c -> if isSpace c then '-' else c) $
            Text.toLower s

getArticleRecord :: Maybe (Key User) -> Key Article -> DBAction (Maybe ArticleRecord)
getArticleRecord maybeUserId articleId = DB.getEntity articleId >>= traverse (mkRecord maybeUserId)

mkRecord :: Maybe (Key User) -> Entity Article -> DBAction ArticleRecord
mkRecord maybeUserId (Entity articleId Article{..}) = do
  articleTagList <-
    map unValue
      <$> ( select $ do
              (articleTag :& tag) <-
                from $
                  table @ArticleTag
                    `innerJoin` table @Tag
                    `on` (\(articleTag :& tag) -> articleTag ^. ArticleTagTagid ==. tag ^. TagId)
              where_ (articleTag ^. ArticleTagArticleid ==. val articleId)
              pure (tag ^. TagName)
          )

  favoritedUsers <-
    map unValue
      <$> ( select $ do
              fav <- from $ table @Favorite
              where_ (fav ^. FavoriteArticleid ==. val articleId)
              pure $ fav ^. FavoriteUserid
          )
  let articleFavorited = maybe False (`elem` favoritedUsers) maybeUserId
  let articleFavoritesCount = length favoritedUsers
  authorProfile <- Profile.getOne maybeUserId articleAuthor

  pure $ ArticleRecord{articleAuthor = authorProfile, ..}

--------------------------------------------------------------------------------

getArticleBySlug :: Maybe (Key User) -> Text -> DBAction (Maybe ArticleRecord)
getArticleBySlug maybeUserId slug =
  DB.getBy (UniqueSlug slug) >>= traverse (mkRecord maybeUserId)

--------------------------------------------------------------------------------

getArticleIdAndAuthorBySlug :: Text -> DBAction (Maybe (Key Article, Key User))
getArticleIdAndAuthorBySlug slug = do
  maybeResult <-
    listToMaybe
      <$> ( select $ do
              article <- from $ table @Article
              where_ (article ^. ArticleSlug ==. val slug)
              pure (article ^. ArticleId, article ^. ArticleAuthor)
          )
  pure $ fmap (\(Value aid, Value uid) -> (aid, uid)) maybeResult

--------------------------------------------------------------------------------

data UpdateArticlePayload = UpdateArticlePayload
  { articleTitle :: Maybe Text
  , articleDescription :: Maybe Text
  , articleBody :: Maybe Text
  }
  deriving stock (Generic)

instance FromJSON UpdateArticlePayload where
  parseJSON = genericParseJSON aesonDropPrefixOptions

instance ToSchema UpdateArticlePayload where
  declareNamedSchema = genericDeclareNamedSchema schemaDropPrefixOptions

update ::
  -- | author
  Key User ->
  Key Article ->
  UpdateArticlePayload ->
  DBAction (Maybe ArticleRecord)
update authorId articleId UpdateArticlePayload{..} = do
  newSlug <- traverse slugify articleTitle
  now <- liftIO getCurrentTime
  let updates =
        catMaybes
          [ ArticleTitle =?. articleTitle
          , ArticleDescription =?. articleDescription
          , ArticleBody =?. articleBody
          , ArticleSlug =?. newSlug
          , ArticleUpdatedAt =?. Just now
          ]
  E.update $ \article -> do
    set article updates
    where_ (article ^. ArticleId ==. val articleId)
    where_ (article ^. ArticleAuthor ==. val authorId)

  getArticleRecord (Just authorId) articleId

--------------------------------------------------------------------------------

delete :: Key User -> Text -> DBAction ()
delete userId slug = do
  let matchSlugAndAuthor article = do
        where_ (article ^. ArticleSlug ==. val slug)
        where_ (article ^. ArticleAuthor ==. val userId)

  E.delete $ do
    articleTag <- from $ table @ArticleTag
    where_ $
      exists $ do
        article <- from $ table @Article
        where_ (article ^. ArticleId ==. articleTag ^. ArticleTagArticleid)
        matchSlugAndAuthor article

  E.delete $ do
    comment <- from $ table @Comment
    where_ $
      exists $ do
        article <- from $ table @Article
        where_ (article ^. ArticleId ==. comment ^. CommentArticle)
        matchSlugAndAuthor article

  E.delete $ do
    fav <- from $ table @Favorite
    where_ $
      exists $ do
        article <- from $ table @Article
        where_ (article ^. ArticleId ==. fav ^. FavoriteArticleid)
        matchSlugAndAuthor article

  E.delete $ do
    article <- from $ table @Article
    matchSlugAndAuthor article

--------------------------------------------------------------------------------
newtype Limit = Limit {getLimit :: Integer}
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Num, FromHttpApiData)

instance ToSchema Limit where
  declareNamedSchema _ =
    plain $
      paramSchemaToSchema (Proxy @Integer)
        & minimum_ ?~ 0
        & maximum_ ?~ 1000
        & default_ ?~ Number 20

newtype Offset = Offset {getOffset :: Integer}
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (Num, FromHttpApiData)

instance ToSchema Offset where
  declareNamedSchema _ =
    plain $
      paramSchemaToSchema (Proxy @Integer)
        & minimum_ ?~ 0
        & maximum_ ?~ 1000
        & default_ ?~ Number 0

data ArticleListInput = ArticleListInput
  { maybeCurrentUserId :: Maybe (Key User)
  , maybeTag :: Maybe Text
  , maybeAuthorName :: Maybe Text
  , maybeFavoritedBy :: Maybe Text
  , listLimit :: Limit
  , listOffset :: Offset
  }

articleList :: ArticleListInput -> DBAction [ArticleRecord]
articleList ArticleListInput{..} = do
  let filterTag article =
        case maybeTag of
          Nothing -> val True
          Just "" -> val True
          Just aTag -> exists $ do
            (articleTag :& tag) <-
              from $
                table @ArticleTag
                  `innerJoin` table @Tag
                  `on` (\(articleTag :& tag) -> articleTag ^. ArticleTagTagid ==. tag ^. TagId)
            where_ (tag ^. TagName ==. val aTag)
            where_ (article ^. ArticleId ==. articleTag ^. ArticleTagArticleid)

      filterAuthor article =
        case maybeAuthorName of
          Nothing -> val True
          Just "" -> val True
          Just authorName -> exists $ do
            user <- from $ table @User
            where_ (article ^. ArticleAuthor ==. user ^. UserId)
            where_ (user ^. UserUsername ==. val authorName)

      filterFavorite article =
        case maybeFavoritedBy of
          Nothing -> val True
          Just "" -> val True
          Just favUserName -> exists $ do
            (fav :& user) <-
              from $
                table @Favorite
                  `innerJoin` table @User
                  `on` (\(fav :& user) -> fav ^. FavoriteUserid ==. user ^. UserId)
            where_ (article ^. ArticleId ==. fav ^. FavoriteArticleid)
            where_ (user ^. UserUsername ==. val favUserName)

  articleIds <- select $ do
    article <- from $ table @Article
    where_ (filterTag article)
    where_ (filterAuthor article)
    where_ (filterFavorite article)
    orderBy [desc $ article ^. ArticleUpdatedAt]
    limit (fromInteger $ getLimit listLimit)
    offset (fromInteger $ getOffset listOffset)
    pure $ article ^. ArticleId

  articles <- traverse (getArticleRecord maybeCurrentUserId . unValue) articleIds
  pure $ catMaybes articles

--------------------------------------------------------------------------------

data ArticleFeedInput = ArticleFeedInput
  { currentUserId :: Key User
  , listLimit :: Limit
  , listOffset :: Offset
  }

articleFeed :: ArticleFeedInput -> DBAction [ArticleRecord]
articleFeed ArticleFeedInput{..} = do
  articleIds <- select $ do
    (article :& follow) <-
      from $
        table @Article
          `innerJoin` table @Follow
          `on` (\(article :& follow) -> follow ^. FollowFollowee ==. article ^. ArticleAuthor)
    where_ (follow ^. FollowFollower ==. val currentUserId)
    orderBy [desc $ article ^. ArticleUpdatedAt]
    limit (fromInteger $ getLimit listLimit)
    offset (fromInteger $ getOffset listOffset)
    pure $ article ^. ArticleId

  articles <- traverse (getArticleRecord (Just currentUserId) . unValue) articleIds
  pure $ catMaybes articles

--------------------------------------------------------------------------------

favorite :: Key User -> Text -> DBAction (Maybe ArticleRecord)
favorite userId slug =
  getArticleIdAndAuthorBySlug slug >>= \case
    Nothing -> pure Nothing
    Just (articleId, _) -> do
      let fav = Favorite{favoriteUserid = userId, favoriteArticleid = articleId}
      let handleDBError :: SqliteException -> DBAction ()
          handleDBError e
            | seError e == ErrorConstraint = pure ()
            | otherwise = throw e
      DB.insert_ fav `catch` handleDBError
      getArticleRecord (Just userId) articleId

--------------------------------------------------------------------------------

unfavorite :: Key User -> Text -> DBAction (Maybe ArticleRecord)
unfavorite userId slug =
  getArticleIdAndAuthorBySlug slug >>= \case
    Nothing -> pure Nothing
    Just (articleId, _) -> do
      DB.deleteBy (UniqueFavorite articleId userId)
      getArticleRecord (Just userId) articleId
