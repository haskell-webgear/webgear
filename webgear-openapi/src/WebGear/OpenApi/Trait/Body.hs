{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Body' trait.
module WebGear.OpenApi.Trait.Body where

import Control.Lens ((&), (.~), (?~))
import Data.OpenApi hiding (Response, contentType)
import Data.OpenApi.Declare (runDeclare)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Exts (fromList)
import WebGear.Core.MIMETypes (MIMEType (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..), ResponseBody)
import WebGear.Core.Trait (Get (..), Set (..), With)
import WebGear.Core.Trait.Body (Body (..), UnknownContentBody (..))
import WebGear.OpenApi.Handler (
  DocNode (DocRequestBody, DocResponseBody),
  OpenApiHandler (..),
  singletonNode,
 )

instance (ToSchema val, MIMEType mt) => Get (OpenApiHandler m) (Body mt val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body mt val -> OpenApiHandler m (Request `With` ts) (Either Text val)
  getTrait (Body mt) =
    let mediaType = mimeType mt
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body =
          (mempty @RequestBody)
            & content .~ fromList [(mediaType, mempty @MediaTypeObject & schema ?~ ref)]
     in OpenApiHandler $ singletonNode (DocRequestBody defs body)

instance (ToSchema val, MIMEType mt) => Set (OpenApiHandler m) (Body mt val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body mt val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body mt val : ts)) ->
    OpenApiHandler m (Response `With` ts, val) (Response `With` (Body mt val : ts))
  setTrait (Body mt) _ =
    let mediaType = mimeType mt
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body = mempty @MediaTypeObject & schema ?~ ref
     in OpenApiHandler $ singletonNode (DocResponseBody defs $ fromList [(mediaType, body)])

instance Set (OpenApiHandler m) UnknownContentBody Response where
  {-# INLINE setTrait #-}
  setTrait ::
    UnknownContentBody ->
    (Response `With` ts -> Response -> ResponseBody -> Response `With` (UnknownContentBody : ts)) ->
    OpenApiHandler m (Response `With` ts, ResponseBody) (Response `With` (UnknownContentBody : ts))
  setTrait UnknownContentBody _ = OpenApiHandler $ singletonNode (DocResponseBody mempty mempty)
