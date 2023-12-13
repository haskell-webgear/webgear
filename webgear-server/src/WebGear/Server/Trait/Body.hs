{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the `Body` trait.
module WebGear.Server.Trait.Body () where

import Control.Arrow (returnA)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (FromByteString, ToByteString (..), parser, runParser')
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack)
import Data.Text.Conversions (FromText (..), ToText (..))
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import Network.Wai (lazyRequestBody)
import WebGear.Core.Handler (Handler (..))
import WebGear.Core.MIMEType.JSON (JSON')
import WebGear.Core.MIMEType.OctetStream (OctetStream)
import WebGear.Core.MIMEType.PlainText (PlainText)
import WebGear.Core.Request (Request (..))
import WebGear.Core.Response (Response (..), ResponseBody (ResponseBodyBuilder))
import WebGear.Core.Trait (Get (..), Set (..), With, unwitness)
import WebGear.Core.Trait.Body (Body (..))
import WebGear.Server.Handler (ServerHandler)

instance
  {-# OVERLAPPABLE #-}
  ( Monad m
  , Get (ServerHandler m) (Body '[mtyp] val) Request
  , Get (ServerHandler m) (Body mtyps val) Request
  ) =>
  Get (ServerHandler m) (Body (mtyp : mtyps) val) Request
  where
  {-# INLINE getTrait #-}
  getTrait :: Body (mtyp : mtyps) val -> ServerHandler m (Request `With` ts) (Either Text val)
  getTrait Body = proc request -> do
    result <- getTrait (Body @'[mtyp] @val) -< request
    case result of
      Left _ -> getTrait (Body @mtyps @val) -< request
      Right t -> returnA -< Right t

bodyToByteString :: (MonadIO m) => Request `With` ts -> m LBS.ByteString
bodyToByteString = liftIO . lazyRequestBody . toWaiRequest . unwitness

instance (MonadIO m, FromByteString val) => Get (ServerHandler m) (Body '[OctetStream] val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body '[OctetStream] val -> ServerHandler m (Request `With` ts) (Either Text val)
  getTrait Body = arrM $ \request -> do
    body <- bodyToByteString request
    pure $ case runParser' parser body of
      Left e -> Left $ pack e
      Right t -> Right t

instance (MonadIO m, FromText val) => Get (ServerHandler m) (Body '[PlainText] val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body '[PlainText] val -> ServerHandler m (Request `With` ts) (Either Text val)
  getTrait Body = arrM $ \request -> do
    body <- bodyToByteString request
    pure $ case LText.decodeUtf8' body of
      Left e -> Left $ pack $ show e
      Right t -> Right $ fromText $ LText.toStrict t

instance (MonadIO m, Aeson.FromJSON val) => Get (ServerHandler m) (Body '[JSON' mt] val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body '[JSON' mt] val -> ServerHandler m (Request `With` ts) (Either Text val)
  getTrait Body = arrM $ \request -> do
    body <- bodyToByteString request
    pure $ case Aeson.eitherDecode' body of
      Left e -> Left $ pack e
      Right t -> Right t

instance (Monad m, ToByteString val) => Set (ServerHandler m) (Body '[OctetStream] val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body '[OctetStream] val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body '[OctetStream] val : ts)) ->
    ServerHandler m (Response `With` ts, val) (Response `With` (Body '[OctetStream] val : ts))
  setTrait Body f = proc (wResponse, val) -> do
    let response = unwitness wResponse
        response' =
          response
            { responseBody = ResponseBodyBuilder (builder val)
            }
    returnA -< f wResponse response' val

instance (Monad m, ToText val) => Set (ServerHandler m) (Body '[PlainText] val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body '[PlainText] val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body '[PlainText] val : ts)) ->
    ServerHandler m (Response `With` ts, val) (Response `With` (Body '[PlainText] val : ts))
  setTrait Body f = proc (wResponse, val) -> do
    let response = unwitness wResponse
        response' =
          response
            { responseBody = ResponseBodyBuilder (Text.encodeUtf8Builder $ toText val)
            }
    returnA -< f wResponse response' val

instance (Monad m, Aeson.ToJSON val) => Set (ServerHandler m) (Body '[JSON' mt] val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body '[JSON' mt] val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body '[JSON' mt] val : ts)) ->
    ServerHandler m (Response `With` ts, val) (Response `With` (Body '[JSON' mt] val : ts))
  setTrait Body f = proc (wResponse, val) -> do
    let response = unwitness wResponse
        response' =
          response
            { responseBody = ResponseBodyBuilder $ Aeson.fromEncoding $ Aeson.toEncoding val
            }
    returnA -< f wResponse response' val

instance (Monad m) => Set (ServerHandler m) (Body '[] ResponseBody) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body '[] ResponseBody ->
    (Response `With` ts -> Response -> ResponseBody -> Response `With` (Body '[] ResponseBody : ts)) ->
    ServerHandler m (Response `With` ts, ResponseBody) (Response `With` (Body '[] ResponseBody : ts))
  setTrait Body f = proc (wResponse, body) -> do
    let response = unwitness wResponse
        response' =
          response
            { responseBody = body
            }
    returnA -< f wResponse response' body

instance (Monad m) => Set (ServerHandler m) (Body '[mt] ResponseBody) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body '[mt] ResponseBody ->
    (Response `With` ts -> Response -> ResponseBody -> Response `With` (Body '[mt] ResponseBody : ts)) ->
    ServerHandler m (Response `With` ts, ResponseBody) (Response `With` (Body '[mt] ResponseBody : ts))
  setTrait Body f = proc (wResponse, body) -> do
    let response = unwitness wResponse
        response' =
          response
            { responseBody = body
            }
    returnA -< f wResponse response' body
