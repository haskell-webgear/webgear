{- |
 Handlers for serving static resources
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
import WebGear.Core.Handler (Handler (..), RoutePath (..), unlinkA)
import WebGear.Core.Request (Request (..))
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Linked (..), Sets)
import WebGear.Core.Trait.Body (Body, setBodyWithoutContentType)
import WebGear.Core.Trait.Header (RequiredHeader, setHeader)
import WebGear.Core.Trait.Status (Status, notFound404, ok200)
import Prelude hiding (readFile)

-- | Serve files under the specified directory.
serveDir ::
  ( MonadIO m
  , Handler h m
  , Sets h [Status, RequiredHeader "Content-Type" Mime.MimeType, Body LBS.ByteString] Response
  ) =>
  -- | The directory to serve
  FilePath ->
  -- | Optional index filename for the root directory. A 404 Not Found
  -- response will be returned for requests to the root path if this
  -- is set to @Nothing@.
  Maybe FilePath ->
  h (Linked req Request) Response
serveDir root index = proc _request -> consumeRoute go -< ()
  where
    go = proc path -> do
      case (path, index) of
        (RoutePath [], Nothing) -> unlinkA <<< notFound404 -< ()
        (RoutePath [], Just f) -> serveFile -< root </> f
        (RoutePath ps, _) -> serveFile -< root </> joinPath (Text.unpack <$> ps)

-- | Serve a file specified by the input filepath.
serveFile ::
  ( MonadIO m
  , Handler h m
  , Sets h [Status, RequiredHeader "Content-Type" Mime.MimeType, Body LBS.ByteString] Response
  ) =>
  h FilePath Response
serveFile = proc file -> do
  maybeContents <- readFile -< file
  case maybeContents of
    Nothing -> unlinkA <<< notFound404 -< ()
    Just contents -> do
      let contentType = Mime.defaultMimeLookup $ Text.pack $ takeFileName file
      r <- ok200 -< ()
      r' <- setBodyWithoutContentType -< (r, contents)
      unlinkA <<< setHeader @"Content-Type" mempty -< (r', contentType)
  where
    readFile = arrM $ \f -> liftIO $ (Just <$> LBS.readFile f) `catchIO` const (pure Nothing)
