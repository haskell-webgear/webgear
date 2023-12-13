{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Body' trait.
module WebGear.OpenApi.Trait.Body where

import Control.Lens ((&), (.~), (?~))
import Data.OpenApi hiding (Response)
import Data.OpenApi.Declare (runDeclare)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Exts (fromList)
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Set (..), With)
import WebGear.Core.Trait.Body (Body (..))
import WebGear.Core.MIMEType (MIMETypes (..))
import WebGear.OpenApi.Handler (
  DocNode (DocRequestBody, DocResponseBody),
  OpenApiHandler (..),
  singletonNode,
 )

instance (ToSchema val, MIMETypes mts) => Get (OpenApiHandler m) (Body mts val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body mts val -> OpenApiHandler m (Request `With` ts) (Either Text val)
  getTrait Body =
    let mediaTypes = mimeTypes $ Proxy @mts
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body =
          (mempty @RequestBody)
            & content .~ fromList (map (,mempty @MediaTypeObject & schema ?~ ref) mediaTypes)
     in OpenApiHandler $ singletonNode (DocRequestBody defs body)

instance (ToSchema val, MIMETypes mts) => Set (OpenApiHandler m) (Body mts val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body mts val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body mts val : ts)) ->
    OpenApiHandler m (Response `With` ts, val) (Response `With` (Body mts val : ts))
  setTrait Body _ =
    let mediaTypes = mimeTypes $ Proxy @mts
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body = mempty @MediaTypeObject & schema ?~ ref
        mts = fromList $ map (,body) mediaTypes
     in OpenApiHandler $ singletonNode (DocResponseBody defs mts)
