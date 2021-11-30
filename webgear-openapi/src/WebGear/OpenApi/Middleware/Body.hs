{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module WebGear.OpenApi.Middleware.Body where

import Control.Lens ((&), (.~), (?~))
import Data.Maybe (fromMaybe)
import Data.OpenApi hiding (Response)
import Data.OpenApi.Declare (runDeclare)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import WebGear.Core.Middleware.Body (Body (..), JSONBody' (JSONBody'), MkMediaType (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Linked, Set (..))
import WebGear.OpenApi.Handler (DocNode (DocRequestBody, DocResponseBody), OpenApiHandler (..),
                                singletonNode)

instance (MkMediaType mediaType, ToSchema val) => Get (OpenApiHandler m) (Body mediaType val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: Body mediaType val -> OpenApiHandler m (Linked ts Request) (Either Text val)
  getTrait Body =
    let
      mediaType = fromMaybe "*/*" $ mkMediaType $ Proxy @mediaType
      (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
      body = (mempty @RequestBody)
             & content .~ [(mediaType, mempty @MediaTypeObject & schema ?~ ref)]
    in
      OpenApiHandler $ singletonNode (DocRequestBody defs body)

instance (MkMediaType mediaType, ToSchema val) => Set (OpenApiHandler m) (Body mediaType val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait :: Body mediaType val
           -> (Linked ts Response -> Response -> val -> Linked (Body mediaType val : ts) Response)
           -> OpenApiHandler m (Linked ts Response, val) (Linked (Body mediaType val : ts) Response)
  setTrait Body _ =
    let
      mediaType = fromMaybe "*/*" $ mkMediaType $ Proxy @mediaType
      (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
      body = mempty @MediaTypeObject & schema ?~ ref
    in
      OpenApiHandler $ singletonNode (DocResponseBody defs mediaType body)

instance (MkMediaType mediaType, ToSchema val) => Get (OpenApiHandler m) (JSONBody' mediaType val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: JSONBody' mediaType val -> OpenApiHandler m (Linked ts Request) (Either Text val)
  getTrait JSONBody' = getTrait (Body @mediaType @val)

instance (MkMediaType mediaType, ToSchema val) => Set (OpenApiHandler m) (JSONBody' mediaType val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait :: JSONBody' mediaType val
           -> (Linked ts Response -> Response -> t -> Linked (JSONBody' mediaType val : ts) Response)
           -> OpenApiHandler m (Linked ts Response, t) (Linked (JSONBody' mediaType val : ts) Response)
  setTrait JSONBody' _ =
    let
      mediaType = fromMaybe "*/*" $ mkMediaType $ Proxy @mediaType
      (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
      body = mempty @MediaTypeObject & schema ?~ ref
    in
      OpenApiHandler $ singletonNode (DocResponseBody defs mediaType body)
