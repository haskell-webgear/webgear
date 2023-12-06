{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'Body' trait.
module WebGear.Swagger.Trait.Body where

import Control.Lens ((&), (.~), (?~))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Swagger hiding (Response)
import Data.Swagger.Declare (runDeclare)
import Data.Text (Text)
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Set (..), With)
import WebGear.Core.Trait.Body (Body (..), JSONBody (..))
import WebGear.Swagger.Handler (
  DocNode (DocRequestBody, DocResponseBody),
  SwaggerHandler (..),
  singletonNode,
 )

instance (ToSchema val) => Get (SwaggerHandler m) (Body val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body val -> SwaggerHandler m (Request `With` ts) (Either Text val)
  getTrait (Body maybeMediaType) =
    let mimeList = MimeList [fromMaybe "*/*" maybeMediaType]
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body =
          mempty @Param
            & schema .~ ParamBody ref
            & required ?~ True
            & name .~ "body"
     in SwaggerHandler $ singletonNode (DocRequestBody defs mimeList body)

instance (ToSchema val) => Set (SwaggerHandler m) (Body val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body val : ts)) ->
    SwaggerHandler m (Response `With` ts, val) (Response `With` (Body val : ts))
  setTrait (Body maybeMediaType) _ =
    let mimeList = MimeList [fromMaybe "*/*" maybeMediaType]
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
     in SwaggerHandler $ singletonNode (DocResponseBody defs mimeList ref)

instance (ToSchema val) => Get (SwaggerHandler m) (JSONBody val) Request where
  {-# INLINE getTrait #-}
  getTrait :: JSONBody val -> SwaggerHandler m (Request `With` ts) (Either Text val)
  getTrait (JSONBody maybeMediaType) = getTrait (Body @val maybeMediaType)

instance (ToSchema val) => Set (SwaggerHandler m) (JSONBody val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    JSONBody val ->
    (Response `With` ts -> Response -> t -> Response `With` (JSONBody val : ts)) ->
    SwaggerHandler m (Response `With` ts, t) (Response `With` (JSONBody val : ts))
  setTrait (JSONBody maybeMediaType) _ =
    let mimeList = MimeList [fromMaybe "*/*" maybeMediaType]
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
     in SwaggerHandler $ singletonNode (DocResponseBody defs mimeList ref)
