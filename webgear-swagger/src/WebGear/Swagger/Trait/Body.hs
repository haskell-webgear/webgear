{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'Body' trait.
module WebGear.Swagger.Trait.Body where

import Control.Lens ((&), (.~), (?~))
import Data.Proxy (Proxy (..))
import Data.Swagger hiding (Response)
import Data.Swagger.Declare (runDeclare)
import Data.Text (Text)
import WebGear.Core.MIMETypes (MIMEType (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..), ResponseBody)
import WebGear.Core.Trait (Get (..), Set (..), With)
import WebGear.Core.Trait.Body (Body (..), UnknownContentBody (..))
import WebGear.Swagger.Handler (
  DocNode (DocRequestBody, DocResponseBody),
  SwaggerHandler (..),
  singletonNode,
 )

instance (ToSchema val, MIMEType mt) => Get (SwaggerHandler m) (Body mt val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body mt val -> SwaggerHandler m (Request `With` ts) (Either Text val)
  getTrait (Body mt) =
    let mimeList = MimeList [mimeType mt]
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body =
          mempty @Param
            & schema .~ ParamBody ref
            & required ?~ True
            & name .~ "body"
     in SwaggerHandler $ singletonNode (DocRequestBody defs mimeList body)

instance (ToSchema val, MIMEType mt) => Set (SwaggerHandler m) (Body mt val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body mt val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body mt val : ts)) ->
    SwaggerHandler m (Response `With` ts, val) (Response `With` (Body mt val : ts))
  setTrait (Body mt) _ =
    let mimeList = MimeList [mimeType mt]
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
     in SwaggerHandler $ singletonNode (DocResponseBody defs mimeList (Just ref))

instance Set (SwaggerHandler m) UnknownContentBody Response where
  {-# INLINE setTrait #-}
  setTrait ::
    UnknownContentBody ->
    (Response `With` ts -> Response -> ResponseBody -> Response `With` (UnknownContentBody : ts)) ->
    SwaggerHandler m (Response `With` ts, ResponseBody) (Response `With` (UnknownContentBody : ts))
  setTrait UnknownContentBody _ = SwaggerHandler $ singletonNode (DocResponseBody mempty mempty Nothing)
