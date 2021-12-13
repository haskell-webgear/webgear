{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Body' trait.
module WebGear.OpenApi.Trait.Body where

import Control.Lens ((&), (.~), (?~))
import Data.Maybe (fromMaybe)
import Data.OpenApi hiding (Response)
import Data.OpenApi.Declare (runDeclare)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import WebGear.Core.Middleware.Body (Body (..), JSONBody (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Linked, Set (..))
import WebGear.OpenApi.Handler (
  DocNode (DocRequestBody, DocResponseBody),
  OpenApiHandler (..),
  singletonNode,
 )

instance ToSchema val => Get (OpenApiHandler m) (Body val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: Body val -> OpenApiHandler m (Linked ts Request) (Either Text val)
  getTrait (Body maybeMediaType) =
    let mediaType = fromMaybe "*/*" maybeMediaType
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body =
          (mempty @RequestBody)
            & content .~ [(mediaType, mempty @MediaTypeObject & schema ?~ ref)]
     in OpenApiHandler $ singletonNode (DocRequestBody defs body)

instance ToSchema val => Set (OpenApiHandler m) (Body val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait ::
    Body val ->
    (Linked ts Response -> Response -> val -> Linked (Body val : ts) Response) ->
    OpenApiHandler m (Linked ts Response, val) (Linked (Body val : ts) Response)
  setTrait (Body maybeMediaType) _ =
    let mediaType = fromMaybe "*/*" maybeMediaType
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body = mempty @MediaTypeObject & schema ?~ ref
     in OpenApiHandler $ singletonNode (DocResponseBody defs mediaType body)

instance ToSchema val => Get (OpenApiHandler m) (JSONBody val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: JSONBody val -> OpenApiHandler m (Linked ts Request) (Either Text val)
  getTrait (JSONBody maybeMediaType) = getTrait (Body @val maybeMediaType)

instance ToSchema val => Set (OpenApiHandler m) (JSONBody val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait ::
    JSONBody val ->
    (Linked ts Response -> Response -> t -> Linked (JSONBody val : ts) Response) ->
    OpenApiHandler m (Linked ts Response, t) (Linked (JSONBody val : ts) Response)
  setTrait (JSONBody maybeMediaType) _ =
    let mediaType = fromMaybe "*/*" maybeMediaType
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body = mempty @MediaTypeObject & schema ?~ ref
     in OpenApiHandler $ singletonNode (DocResponseBody defs mediaType body)
