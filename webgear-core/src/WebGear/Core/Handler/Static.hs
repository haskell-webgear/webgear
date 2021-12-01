{- |
 Handler serving static resources
-}
module WebGear.Core.Handler.Static (
  serveDir,
  serveFile,
) where

import Control.Arrow ((<<<))
import Control.Exception.Safe (catchIO)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.Mime as Mime
import System.FilePath (joinPath, takeFileName, (</>))
import WebGear.Core.Handler (Handler (..), RoutePath (..), respondA, (>#>))
import WebGear.Core.Middleware.Body (Body, setBodyWithoutContentType)
import WebGear.Core.Middleware.Header (RequiredHeader, setHeader)
import WebGear.Core.Middleware.Status (Status, notFound404, ok200)
import WebGear.Core.Request (Request (..))
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Linked (..), Sets)
import Prelude hiding (readFile)

serveDir ::
  ( MonadIO m
  , Handler h m
  , Sets
      h
      [ Status
      , RequiredHeader "Content-Type" Mime.MimeType
      , Body Nothing LBS.ByteString
      ]
      Response
  ) =>
  -- | directory to serve
  FilePath ->
  -- | index filename for the root directory
  Maybe FilePath ->
  h (Linked req Request) Response
serveDir root index = proc _request -> consumeRoute go -< ()
  where
    go = proc path -> do
      case (path, index) of
        (RoutePath [], Nothing) -> respondA <<< notFound404 -< ()
        (RoutePath [], Just f) -> serveFile -< root </> f
        (RoutePath ps, _) -> serveFile -< root </> joinPath (Text.unpack <$> ps)

serveFile ::
  ( MonadIO m
  , Handler h m
  , Sets
      h
      [ Status
      , RequiredHeader "Content-Type" Mime.MimeType
      , Body Nothing LBS.ByteString
      ]
      Response
  ) =>
  h FilePath Response
serveFile = proc file -> do
  maybeContents <- readFile -< file
  case maybeContents of
    Nothing -> respondA <<< notFound404 -< ()
    Just contents -> do
      let contentType = Mime.defaultMimeLookup $ Text.pack $ takeFileName file
      r <-
        (ok200 -< ()) >#>
          (\r -> setBodyWithoutContentType -< (r, contents)) >#>
          (\r -> setHeader @"Content-Type" -< (r, contentType))
      respondA -< r
  where
    readFile = handler $ \f -> liftIO $ (Just <$> LBS.readFile f) `catchIO` const (pure Nothing)
