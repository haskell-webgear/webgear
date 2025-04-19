{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import qualified API.Article as Article
import qualified API.Comment as Comment
import API.Common (App (..), AppEnv (..))
import qualified API.Profile as Profile
import qualified API.Tag as Tag
import qualified API.UI as UI
import qualified API.User as User
import Control.Monad.Reader (runReaderT)
import qualified Crypto.JWT as JWT
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.SQLite.Simple (Connection)
import Model.Common (withDBConnectionPool)
import Network.HTTP.Types (StdMethod (..))
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import WebGear.OpenApi (toOpenApi)
import WebGear.Server
import WebGear.Swagger.UI (swaggerUI)

--------------------------------------------------------------------------------
-- A medium.com clone app specified by https://github.com/gothinkster/realworld
--------------------------------------------------------------------------------

application :: Pool Connection -> JWT.JWK -> Wai.Application
application pool jwk = toApplication $ transform appToIO allRoutes
  where
    allRoutes :: RequestHandler (ServerHandler App) '[]
    allRoutes =
      appRoutes
        <+> [match| /openapi |] (swaggerUI $ toOpenApi @App $ appRoutes)
        <+> uiRoutes

    appRoutes =
      [route|       POST    /api/users/login                                 |] (User.login jwk)
        <+> [route| POST    /api/users                                       |] (User.create jwk)
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

    uiRoutes :: RequestHandler (ServerHandler App) '[]
    uiRoutes = method GET UI.assets

    appToIO :: App a -> IO a
    appToIO = flip runReaderT (AppEnv pool) . unApp

main :: IO ()
main = withDBConnectionPool $ \pool -> do
  jwkBS <- LBS.readFile "realworld.jwk"
  let jwk = either error id $ eitherDecode jwkBS
  Warp.run 3000 (application pool jwk)
