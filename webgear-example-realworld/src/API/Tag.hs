module API.Tag (
  list,
) where

import API.Common
import Control.Category ((.))
import qualified Model.Tag as Model
import qualified Network.HTTP.Types as HTTP
import Relude hiding ((.))
import WebGear.Server

type TagsResponse = Wrapped "tags" [Text]

list ::
  ( StdHandler h App
  , Sets h [RequiredResponseHeader "Content-Type" Text, JSONBody TagsResponse] Response
  ) =>
  RequestHandler h req
list =
  withDoc "Get all tags" ""
    $ proc _request -> do
      tags <- fetchTags -< ()
      respondJsonA HTTP.ok200 . setDescription okDescription -< Wrapped tags :: TagsResponse
  where
    fetchTags = arrM $ const $ runDBAction Model.list
