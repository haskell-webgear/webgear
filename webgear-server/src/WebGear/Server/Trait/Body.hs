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
import WebGear.Core.Request (Request, getRequestBodyChunk)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Set (..), With, unwitness)
import WebGear.Core.Trait.Body (Body (..), JSONBody (..))
import WebGear.Server.Handler (ServerHandler)

instance (MonadIO m, FromByteString val) => Get (ServerHandler m) (Body val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body val -> ServerHandler m (Request `With` ts) (Either Text val)
  getTrait (Body _) = arrM $ \request -> do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ getRequestBodyChunk $ unwitness request
    pure $ case runParser' parser (fromChunks chunks) of
      Left e -> Left $ pack e
      Right t -> Right t

instance (Monad m, ToByteString val) => Set (ServerHandler m) (Body val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body val : ts)) ->
    ServerHandler m (Response `With` ts, val) (Response `With` (Body val : ts))
  setTrait (Body mediaType) f = proc (wResponse, val) -> do
    let response = unwitness wResponse
        response' =
          response
            { responseBody = Just (toByteString val)
            , responseHeaders =
                responseHeaders response
                  <> case mediaType of
                    Just mt -> [(hContentType, renderHeader mt)]
                    Nothing -> []
            }
    returnA -< f wResponse response' val

instance (MonadIO m, Aeson.FromJSON val) => Get (ServerHandler m) (JSONBody val) Request where
  {-# INLINE getTrait #-}
  getTrait :: JSONBody val -> ServerHandler m (Request `With` ts) (Either Text val)
  getTrait (JSONBody _) = arrM $ \request -> do
    chunks <- takeWhileM (/= mempty) $ repeat $ liftIO $ getRequestBodyChunk $ unwitness request
    pure $ case Aeson.eitherDecode' (fromChunks chunks) of
      Left e -> Left $ pack e
      Right t -> Right t

instance (Monad m, Aeson.ToJSON val) => Set (ServerHandler m) (JSONBody val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    JSONBody val ->
    (Response `With` ts -> Response -> val -> Response `With` (JSONBody val : ts)) ->
    ServerHandler m (Response `With` ts, val) (Response `With` (JSONBody val : ts))
  setTrait (JSONBody mediaType) f = proc (wResponse, val) -> do
    let response = unwitness wResponse
        ctype = maybe "application/json" renderHeader mediaType
        response' =
          response
            { responseBody = Just (Aeson.encode val)
            , responseHeaders =
                responseHeaders response
                  <> [(hContentType, ctype)]
            }
    returnA -< f wResponse response' val

takeWhileM :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
takeWhileM _ [] = pure []
takeWhileM p (mx : mxs) = do
  x <- mx
  if p x
    then (x :) <$> takeWhileM p mxs
    else pure []
