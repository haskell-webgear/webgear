{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the `Body` trait.
module WebGear.Server.Trait.Body () where

import Control.Monad.Trans (lift)
import Data.Text (Text)
import qualified Network.HTTP.Media as HTTP
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Handler (..))
import WebGear.Core.Request (Request (..))
import WebGear.Core.Response (Response (..), ResponseBody)
import WebGear.Core.Trait (Get (..), Set (..), With, unwitness)
import WebGear.Core.Trait.Body (Body (..), UnknownContentBody (..))
import WebGear.Server.Handler (ServerHandler (..))
import WebGear.Server.MIMETypes (BodyRender (..), BodyUnrender (..))

instance (Monad m, BodyUnrender m mt val) => Get (ServerHandler m) (Body mt val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body mt val -> ServerHandler m (Request `With` ts) (Either Text val)
  getTrait (Body mt) = arrM $ bodyUnrender mt . unwitness

instance (Monad m, BodyRender m mt val) => Set (ServerHandler m) (Body mt val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body mt val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body mt val : ts)) ->
    ServerHandler m (Response `With` ts, val) (Response `With` (Body mt val : ts))
  setTrait (Body mt) f = ServerHandler $ \(wResponse, val) -> do
    let response = unwitness wResponse
    case response of
      Response status hdrs _ -> do
        (mediaType, body') <- lift $ lift $ bodyRender mt response val
        let response' = Response status (alterContentType mediaType hdrs) body'
        pure $ f wResponse response' val
      _ -> pure $ f wResponse response val

alterContentType :: HTTP.MediaType -> HTTP.ResponseHeaders -> HTTP.ResponseHeaders
alterContentType mt = go
  where
    mtStr = HTTP.renderHeader mt
    go [] = [(HTTP.hContentType, mtStr)]
    go ((n, v) : hdrs)
      | n == HTTP.hContentType = (HTTP.hContentType, mtStr) : hdrs
      | otherwise = (n, v) : go hdrs

instance (Monad m) => Set (ServerHandler m) UnknownContentBody Response where
  {-# INLINE setTrait #-}
  setTrait ::
    UnknownContentBody ->
    (Response `With` ts -> Response -> ResponseBody -> Response `With` (UnknownContentBody : ts)) ->
    ServerHandler m (Response `With` ts, ResponseBody) (Response `With` (UnknownContentBody : ts))
  setTrait UnknownContentBody f = ServerHandler $ \(wResponse, body') ->
    case unwitness wResponse of
      Response status hdrs _ -> pure $ f wResponse (Response status hdrs body') body'
      response -> pure $ f wResponse response body'
