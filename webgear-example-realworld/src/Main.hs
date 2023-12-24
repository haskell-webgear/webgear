{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main where

import qualified API.Article as Article
import qualified API.Comment as Comment
import API.Common (App (..), AppEnv (..))
import qualified API.Profile as Profile
import qualified API.Tag as Tag
import qualified API.UI as UI
import qualified API.User as User
import Control.Category ((.))
import qualified Crypto.JWT as JWT
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import Model.Common (withDBConnectionPool)
import Network.HTTP.Types (StdMethod (..))
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Relude hiding ((.))
import WebGear.OpenApi (toOpenApi)
import WebGear.Server
import WebGear.Swagger.UI (swaggerUI)

--------------------------------------------------------------------------------
-- A medium.com clone app specified by https://github.com/gothinkster/realworld
--------------------------------------------------------------------------------

appRoutes jwk =
  [route| POST    /api/users/login                                 |] (User.login jwk)
    <+> [route|       POST    /api/users                                       |] (User.create jwk)
    <+> [route| GET     /api/user                                        |] (User.current jwk)
    <+> [route| PUT     /api/user                                        |] (User.update jwk)
    <+> [route| GET     /api/profiles/username:Text                      |] (Profile.getByName jwk)
    <+> [route| POST    /api/profiles/username:Text/follow               |] (Profile.follow jwk)
    <+> [route| DELETE  /api/profiles/username:Text/follow               |] (Profile.unfollow jwk)
    <+> [route| POST    /api/articles                                    |] (Article.create jwk)
    <+> [route| GET     /api/articles                                    |] (Article.list jwk)
    <+> [route| GET     /api/articles/feed                               |] (Article.feed jwk)
    <+> [route| GET     /api/articles/slug:Text                          |] (Article.getBySlug jwk)
    <+> [route| PUT     /api/articles/slug:Text                          |] (Article.update jwk)
    <+> [route| DELETE  /api/articles/slug:Text                          |] (Article.delete jwk)
    <+> [route| POST    /api/articles/slug:Text/favorite                 |] (Article.favorite jwk)
    <+> [route| DELETE  /api/articles/slug:Text/favorite                 |] (Article.unfavorite jwk)
    <+> [route| POST    /api/articles/slug:Text/comments                 |] (Comment.create jwk)
    <+> [route| GET     /api/articles/slug:Text/comments                 |] (Comment.list jwk)
    <+> [route| DELETE  /api/articles/slug:Text/comments/commentId:Int64 |] (Comment.delete jwk)
    <+> [route| GET     /api/tags                                        |] Tag.list

application :: Pool SqlBackend -> JWT.JWK -> Wai.Application
application pool jwk = toApplication $ transform appToIO allRoutes
  where
    allRoutes =
      appRoutes jwk
        <+> uiRoutes
        <+> [match| /openapi |] (swaggerUI $ toOpenApi @App $ appRoutes jwk)

    uiRoutes =
      [match|       GET  /ui/assets |] UI.assets
        <+> [match| GET  /ui        |] UI.index
        <+> [route| GET  /          |] UI.index

    appToIO :: App a -> IO a
    appToIO = flip runReaderT (AppEnv pool) . unApp

main :: IO ()
main = withDBConnectionPool $ \pool -> do
  jwkBS <- LBS.readFile "realworld.jwk"
  let jwk = either (error . toText) id $ eitherDecode jwkBS
  Warp.run 3000 (application pool jwk)
