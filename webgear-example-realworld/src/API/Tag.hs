module API.Tag (
  list,
) where

import API.Common
import Data.Text (Text)
import qualified Model.Tag as Model
import qualified Network.HTTP.Types as HTTP
import WebGear.Server

type TagsResponse = Wrapped "tags" [Text]

list ::
  ( StdHandler h App
  , Set h (JSONBody TagsResponse)
  ) =>
  RequestHandler h ts
list =
  withDoc "Get all tags" "" $
    proc _request -> do
      tags <- fetchTags -< ()
      setDescription okDescription <<< respondJsonA HTTP.ok200 -< Wrapped tags :: TagsResponse
  where
    fetchTags = arrM $ const $ runDBAction Model.list
