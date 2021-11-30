{-# OPTIONS_GHC -Wno-orphans #-}

module WebGear.Server.Middleware.Body where

import Control.Arrow (returnA)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (FromByteString, ToByteString, parser, runParser', toByteString)
import Data.ByteString.Lazy (fromChunks)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack)
import Network.HTTP.Media.RenderHeader (RenderHeader (renderHeader))
import Network.HTTP.Types (hContentType)
import WebGear.Core.Handler (Handler (..))
import WebGear.Core.Middleware.Body (Body (..), JSONBody' (..), MkMediaType (..))
import WebGear.Core.Request (Request, getRequestBodyChunk)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Linked, Set (..), unlink)
import WebGear.Server.Handler (ServerHandler)

instance (MonadIO m, FromByteString val) => Get (ServerHandler m) (Body mediaType val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: Body mediaType val -> ServerHandler m (Linked ts Request) (Either Text val)
  getTrait Body = handler $ \request -> do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ getRequestBodyChunk $ unlink request
    pure $ case runParser' parser (fromChunks chunks) of
      Left e -> Left $ pack e
      Right t -> Right t

instance (Monad m, MkMediaType mediaType, ToByteString val) => Set (ServerHandler m) (Body mediaType val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait ::
    Body mediaType val ->
    (Linked ts Response -> Response -> val -> Linked (Body mediaType val : ts) Response) ->
    ServerHandler m (Linked ts Response, val) (Linked (Body mediaType val : ts) Response)
  setTrait Body f = proc (linkedResponse, val) -> do
    let response = (unlink linkedResponse)
        response' =
          response
            { responseBody = Just (toByteString val)
            , responseHeaders =
                responseHeaders response
                  <> case mkMediaType (Proxy @mediaType) of
                    Just mt -> [(hContentType, renderHeader mt)]
                    Nothing -> []
            }
    returnA -< f linkedResponse response' val

instance (MonadIO m, Aeson.FromJSON val) => Get (ServerHandler m) (JSONBody' mediaType val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: JSONBody' mediaType val -> ServerHandler m (Linked ts Request) (Either Text val)
  getTrait JSONBody' = handler $ \request -> do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ getRequestBodyChunk $ unlink request
    pure $ case Aeson.eitherDecode' (fromChunks chunks) of
      Left e -> Left $ pack e
      Right t -> Right t

instance (Monad m, MkMediaType mediaType, Aeson.ToJSON val) => Set (ServerHandler m) (JSONBody' mediaType val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait ::
    JSONBody' mediaType val ->
    (Linked ts Response -> Response -> val -> Linked (JSONBody' mediaType val : ts) Response) ->
    ServerHandler m (Linked ts Response, val) (Linked (JSONBody' mediaType val : ts) Response)
  setTrait JSONBody' f = proc (linkedResponse, val) -> do
    let response = unlink linkedResponse
        ctype = maybe "application/json" renderHeader $ mkMediaType (Proxy @mediaType)
        response' =
          response
            { responseBody = Just (Aeson.encode val)
            , responseHeaders =
                responseHeaders response
                  <> [(hContentType, ctype)]
            }
    returnA -< f linkedResponse response' val

takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM _ [] = pure []
takeWhileM p (mx : mxs) = do
  x <- mx
  if p x
    then (x :) <$> takeWhileM p mxs
    else pure []
