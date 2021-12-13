{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the `Body` trait.
module WebGear.Server.Trait.Body () where

import Control.Arrow (returnA)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (FromByteString, ToByteString, parser, runParser', toByteString)
import Data.ByteString.Lazy (fromChunks)
import Data.Text (Text, pack)
import Network.HTTP.Media.RenderHeader (RenderHeader (renderHeader))
import Network.HTTP.Types (hContentType)
import WebGear.Core.Handler (Handler (..))
import WebGear.Core.Middleware.Body (Body (..), JSONBody (..))
import WebGear.Core.Request (Request, getRequestBodyChunk)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Linked, Set (..), unlink)
import WebGear.Server.Handler (ServerHandler)

instance (MonadIO m, FromByteString val) => Get (ServerHandler m) (Body val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: Body val -> ServerHandler m (Linked ts Request) (Either Text val)
  getTrait (Body _) = arrM $ \request -> do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ getRequestBodyChunk $ unlink request
    pure $ case runParser' parser (fromChunks chunks) of
      Left e -> Left $ pack e
      Right t -> Right t

instance (Monad m, ToByteString val) => Set (ServerHandler m) (Body val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait ::
    Body val ->
    (Linked ts Response -> Response -> val -> Linked (Body val : ts) Response) ->
    ServerHandler m (Linked ts Response, val) (Linked (Body val : ts) Response)
  setTrait (Body mediaType) f = proc (linkedResponse, val) -> do
    let response = (unlink linkedResponse)
        response' =
          response
            { responseBody = Just (toByteString val)
            , responseHeaders =
                responseHeaders response
                  <> case mediaType of
                    Just mt -> [(hContentType, renderHeader mt)]
                    Nothing -> []
            }
    returnA -< f linkedResponse response' val

instance (MonadIO m, Aeson.FromJSON val) => Get (ServerHandler m) (JSONBody val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: JSONBody val -> ServerHandler m (Linked ts Request) (Either Text val)
  getTrait (JSONBody _) = arrM $ \request -> do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ getRequestBodyChunk $ unlink request
    pure $ case Aeson.eitherDecode' (fromChunks chunks) of
      Left e -> Left $ pack e
      Right t -> Right t

instance (Monad m, Aeson.ToJSON val) => Set (ServerHandler m) (JSONBody val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait ::
    JSONBody val ->
    (Linked ts Response -> Response -> val -> Linked (JSONBody val : ts) Response) ->
    ServerHandler m (Linked ts Response, val) (Linked (JSONBody val : ts) Response)
  setTrait (JSONBody mediaType) f = proc (linkedResponse, val) -> do
    let response = unlink linkedResponse
        ctype = maybe "application/json" renderHeader mediaType
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
