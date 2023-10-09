{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Body' trait.
module WebGear.OpenApi.Trait.Body where

import Control.Lens ((&), (.~), (?~))
import Data.Maybe (fromMaybe)
import Data.OpenApi hiding (Response)
import Data.OpenApi.Declare (runDeclare)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Set (..), With)
import WebGear.Core.Trait.Body (Body (..), JSONBody (..))
import WebGear.OpenApi.Handler (
  DocNode (DocRequestBody, DocResponseBody),
  OpenApiHandler (..),
  singletonNode,
 )

instance (ToSchema val) => Get (OpenApiHandler m) (Body val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body val -> OpenApiHandler m (Request `With` ts) (Either Text val)
  getTrait (Body maybeMediaType) =
    let mediaType = fromMaybe "*/*" maybeMediaType
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body =
          (mempty @RequestBody)
            & content .~ [(mediaType, mempty @MediaTypeObject & schema ?~ ref)]
     in OpenApiHandler $ singletonNode (DocRequestBody defs body)

instance (ToSchema val) => Set (OpenApiHandler m) (Body val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body val : ts)) ->
    OpenApiHandler m (Response `With` ts, val) (Response `With` (Body val : ts))
  setTrait (Body maybeMediaType) _ =
    let mediaType = fromMaybe "*/*" maybeMediaType
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body = mempty @MediaTypeObject & schema ?~ ref
     in OpenApiHandler $ singletonNode (DocResponseBody defs mediaType body)

instance (ToSchema val) => Get (OpenApiHandler m) (JSONBody val) Request where
  {-# INLINE getTrait #-}
  getTrait :: JSONBody val -> OpenApiHandler m (Request `With` ts) (Either Text val)
  getTrait (JSONBody maybeMediaType) = getTrait (Body @val maybeMediaType)

instance (ToSchema val) => Set (OpenApiHandler m) (JSONBody val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    JSONBody val ->
    (Response `With` ts -> Response -> t -> Response `With` (JSONBody val : ts)) ->
    OpenApiHandler m (Response `With` ts, t) (Response `With` (JSONBody val : ts))
  setTrait (JSONBody maybeMediaType) _ =
    let mediaType = fromMaybe "*/*" maybeMediaType
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body = mempty @MediaTypeObject & schema ?~ ref
     in OpenApiHandler $ singletonNode (DocResponseBody defs mediaType body)
