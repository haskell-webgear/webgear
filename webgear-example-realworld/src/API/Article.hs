module API.Article (
  create,
  getBySlug,
  update,
  delete,
  list,
  feed,
  favorite,
  unfavorite,
) where

import API.Common
import Control.Exception.Safe (try)
import qualified Crypto.JWT as JWT
import Data.Aeson (ToJSON)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import qualified Database.SQLite.Simple as DB
import GHC.Generics (Generic)
import qualified Model.Article as Model
import Model.Entities
import qualified Network.HTTP.Types as HTTP
import WebGear.Server

type CreateArticleRequest = Wrapped "article" Model.CreateArticlePayload
type ArticleResponse = Wrapped "article" Model.ArticleRecord

create ::
  ( StdHandler h App
  , Gets h [AuthHeader, RequiredAuth, JSONBody CreateArticleRequest]
  , Sets h (JSONBodyOrError ArticleResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
create jwk =
  withDoc "Create new article" "Add a new article to the store" $
    requiredTokenAuth jwk $
      jsonRequestBody @CreateArticleRequest badRequestBody $
        proc request -> do
          result <- createArticle -< request
          case result of
            Left e -> handleDBError -< e
            Right Nothing ->
              unwitnessA <<< setDescription (resp404Description "Article") <<< notFound404 -< ()
            Right (Just article) ->
              setDescription okDescription <<< respondJsonA HTTP.ok200 -< Wrapped article :: ArticleResponse
  where
    createArticle = arrM $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          articlePayload = pick @(JSONBody CreateArticleRequest) $ from request
      try $ runDBAction $ Model.create currentUserId $ unwrap articlePayload

handleDBError ::
  ( StdHandler h App
  , Set h (JSONBody ErrorResponse)
  ) =>
  h DB.SQLError Response
handleDBError = proc e ->
  if DB.sqlError e == DB.ErrorConstraint
    then setDescription (dupDescription "article") <<< respondJsonA HTTP.badRequest400 -< "Article already exists" :: ErrorResponse
    else respondJsonA HTTP.internalServerError500 -< showError e

--------------------------------------------------------------------------------

getBySlug ::
  ( StdHandler h App
  , Gets h [AuthHeader, OptionalAuth]
  , Set h (JSONBody ArticleResponse)
  , HasTrait PathVarSlug ts
  ) =>
  JWT.JWK ->
  RequestHandler h ts
getBySlug jwk =
  withDoc "Retrieve an article" "Fetch an article by its slug" $
    optionalTokenAuth jwk $
      proc request -> do
        maybeArticle <- fetchArticle -< request
        case maybeArticle of
          Nothing ->
            unwitnessA <<< setDescription (resp404Description "Article") <<< notFound404 -< ()
          Just article ->
            setDescription okDescription <<< respondJsonA HTTP.ok200 -< Wrapped article :: ArticleResponse
  where
    fetchArticle = arrM $ \request -> do
      let maybeCurrentUserId = rightToMaybe $ pick @OptionalAuth $ from request
          slug = pick @PathVarSlug $ from request
      runDBAction $ Model.getArticleBySlug maybeCurrentUserId slug

--------------------------------------------------------------------------------

type UpdateArticleRequest = Wrapped "article" Model.UpdateArticlePayload

update ::
  forall h ts.
  ( HasTrait PathVarSlug ts
  , StdHandler h App
  , Gets h [AuthHeader, RequiredAuth, JSONBody UpdateArticleRequest]
  , Sets h (JSONBodyOrError ArticleResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
update jwk =
  withDoc "Update an article" "Only the author can update their articles" $
    requiredTokenAuth jwk $
      jsonRequestBody @UpdateArticleRequest badRequestBody $
        proc request -> do
          let userId = pick @RequiredAuth $ from request
              updatePayload = pick @(JSONBody UpdateArticleRequest) $ from request
              articleSlug = pick @PathVarSlug $ from request

          fetchResult <- getArticleIdAndAuthor -< articleSlug
          case fetchResult of
            Nothing -> unwitnessA <<< setDescription (resp404Description "Article") <<< notFound404 -< ()
            Just (articleId, authorId)
              | authorId /= userId ->
                  setDescription resp403Description <<< respondJsonA HTTP.forbidden403 -< "Permission denied" :: ErrorResponse
              | otherwise -> do
                  updateResult <- updateArticle -< (authorId, articleId, unwrap updatePayload)
                  case updateResult of
                    Left e ->
                      handleDBError -< e
                    Right Nothing ->
                      unwitnessA <<< setDescription (resp404Description "Article") <<< notFound404 -< ()
                    Right (Just article) ->
                      setDescription okDescription <<< respondJsonA HTTP.ok200 -< Wrapped article :: ArticleResponse
  where
    getArticleIdAndAuthor :: h Text (Maybe (ArticleId, UserId))
    getArticleIdAndAuthor = arrM $ \slug -> runDBAction (Model.getArticleIdAndAuthorBySlug slug)

    updateArticle :: h (UserId, ArticleId, Model.UpdateArticlePayload) (Either DB.SQLError (Maybe Model.ArticleRecord))
    updateArticle = arrM $ \(authorId, articleId, updatePayload) ->
      try $ runDBAction $ Model.update authorId articleId updatePayload

--------------------------------------------------------------------------------

delete ::
  ( HasTrait PathVarSlug ts
  , StdHandler h App
  , Gets h [AuthHeader, RequiredAuth]
  , Set h (JSONBody ErrorResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
delete jwk =
  withDoc "Delete an article" "Only the auther can delete their articles" $
    requiredTokenAuth jwk $
      proc request -> do
        let currentUserId = pick @RequiredAuth $ from request
            slug = pick @PathVarSlug $ from request
        _ <- deleteArticle -< (currentUserId, slug)
        unwitnessA <<< setDescription okDescription <<< noContent204 -< ()
  where
    deleteArticle = arrM $ \(currentUserId, slug) ->
      runDBAction $ Model.delete currentUserId slug

--------------------------------------------------------------------------------

data ArticleListResponse = ArticleListResponse
  { articles :: ![Model.ArticleRecord]
  , articlesCount :: !Int
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, ToSchema)

param ::
  forall name val h ts.
  ( StdHandler h App
  , Get h (OptionalQueryParam name val)
  , Set h (JSONBody ErrorResponse)
  ) =>
  Description ->
  Middleware h ts (OptionalQueryParam name val : ts)
param descr nextHandler = optionalQueryParam badRequestParam $ nextHandler <<< setDescription descr

list ::
  ( StdHandler h App
  , Gets
      h
      [ AuthHeader
      , OptionalAuth
      , OptionalQueryParam "tag" Text
      , OptionalQueryParam "author" Text
      , OptionalQueryParam "favorited" Text
      , OptionalQueryParam "limit" Model.Limit
      , OptionalQueryParam "offset" Model.Offset
      ]
  , Sets h (JSONBodyOrError ArticleListResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
list jwk =
  withDoc "Search articles" "Filter articles by query parameters" $
    optionalTokenAuth jwk $
      param @"tag" @Text "Article tag to filter" $
        param @"author" @Text "Name of Author" $
          param @"favorited" @Text "Favorited by user" $
            param @"limit" @Model.Limit "Number of articles" $
              param @"offset" @Model.Offset "Offset of articles" $
                proc request -> do
                  articles <- listArticles -< request
                  let resp = ArticleListResponse articles (length articles)
                  setDescription okDescription <<< respondJsonA HTTP.ok200 -< resp
  where
    listArticles = arrM $ \request -> do
      let maybeCurrentUserId = rightToMaybe $ pick @OptionalAuth $ from request
          maybeTag = pick @(OptionalQueryParam "tag" Text) $ from request
          maybeAuthorName = pick @(OptionalQueryParam "author" Text) $ from request
          maybeFavoritedBy = pick @(OptionalQueryParam "favorited" Text) $ from request
          listLimit = fromMaybe 20 $ pick @(OptionalQueryParam "limit" Model.Limit) $ from request
          listOffset = fromMaybe 0 $ pick @(OptionalQueryParam "offset" Model.Offset) $ from request

      runDBAction $ Model.articleList Model.ArticleListInput{..}

--------------------------------------------------------------------------------

feed ::
  ( StdHandler h App
  , Gets
      h
      [ AuthHeader
      , RequiredAuth
      , OptionalQueryParam "limit" Model.Limit
      , OptionalQueryParam "offset" Model.Offset
      ]
  , Sets h (JSONBodyOrError ArticleListResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
feed jwk =
  withDoc "Feed of articles" "Get a list of article the current user follows" $
    requiredTokenAuth jwk $
      param @"limit" @Model.Limit "Number of articles" $
        param @"offset" @Model.Offset "Offset of articles" $
          proc request -> do
            articles <- getArticles -< request
            let resp = ArticleListResponse articles (length articles)
            setDescription okDescription <<< respondJsonA HTTP.ok200 -< resp
  where
    getArticles = arrM $ \request -> do
      let currentUserId = pick @RequiredAuth $ from request
          listLimit = fromMaybe 20 $ pick @(OptionalQueryParam "limit" Model.Limit) $ from request
          listOffset = fromMaybe 0 $ pick @(OptionalQueryParam "offset" Model.Offset) $ from request

      runDBAction $ Model.articleFeed Model.ArticleFeedInput{..}

--------------------------------------------------------------------------------

favorite ::
  ( HasTrait PathVarSlug ts
  , StdHandler h App
  , Gets h [AuthHeader, RequiredAuth]
  , Sets h (JSONBodyOrError ArticleResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
favorite jwk =
  withDoc "Favorite article" "Flag an article as favorite" $
    requiredTokenAuth jwk $
      proc request -> do
        let currentUserId = pick @RequiredAuth $ from request
            slug = pick @PathVarSlug $ from request
        result <- doFavorite -< (currentUserId, slug)
        case result of
          Nothing ->
            unwitnessA <<< setDescription (resp404Description "Article") <<< notFound404 -< ()
          Just article ->
            setDescription okDescription <<< respondJsonA HTTP.ok200 -< Wrapped article :: ArticleResponse
  where
    doFavorite = arrM $ \(userId, slug) ->
      runDBAction $ Model.favorite userId slug

--------------------------------------------------------------------------------

unfavorite ::
  ( HasTrait PathVarSlug ts
  , StdHandler h App
  , Gets h [AuthHeader, RequiredAuth]
  , Sets h (JSONBodyOrError ArticleResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
unfavorite jwk =
  withDoc "Unfavorite article" "Remove the favorite flag from an article" $
    requiredTokenAuth jwk $
      proc request -> do
        let currentUserId = pick @RequiredAuth $ from request
            slug = pick @PathVarSlug $ from request
        result <- doUnfavorite -< (currentUserId, slug)
        case result of
          Nothing ->
            unwitnessA <<< setDescription (resp404Description "Article") <<< notFound404 -< ()
          Just article ->
            setDescription okDescription <<< respondJsonA HTTP.ok200 -< Wrapped article :: ArticleResponse
  where
    doUnfavorite = arrM $ \(userId, slug) ->
      runDBAction $ Model.unfavorite userId slug
