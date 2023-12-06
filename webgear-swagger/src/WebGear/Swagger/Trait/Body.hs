{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'Body' trait.
module WebGear.Swagger.Trait.Body where

import Control.Lens ((&), (.~), (?~))
import Data.Proxy (Proxy (..))
import Data.Swagger hiding (Response)
import Data.Swagger.Declare (runDeclare)
import Data.Text (Text)
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Get (..), Set (..), With)
import WebGear.Core.Trait.Body (Body (..), MIMETypes, mimeTypes)
import WebGear.Swagger.Handler (
  DocNode (DocRequestBody, DocResponseBody),
  SwaggerHandler (..),
  singletonNode,
 )

instance (ToSchema val, MIMETypes mts) => Get (SwaggerHandler m) (Body mts val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body mts val -> SwaggerHandler m (Request `With` ts) (Either Text val)
  getTrait Body =
    let mimeList = MimeList $ mimeTypes $ Proxy @mts
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body =
          mempty @Param
            & schema .~ ParamBody ref
            & required ?~ True
            & name .~ "body"
     in SwaggerHandler $ singletonNode (DocRequestBody defs mimeList body)

instance (ToSchema val, MIMETypes mts) => Set (SwaggerHandler m) (Body mts val) Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body mts val ->
    (Response `With` ts -> Response -> val -> Response `With` (Body mts val : ts)) ->
    SwaggerHandler m (Response `With` ts, val) (Response `With` (Body mts val : ts))
  setTrait Body _ =
    let mimeList = MimeList $ mimeTypes $ Proxy @mts
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
     in SwaggerHandler $ singletonNode (DocResponseBody defs mimeList ref)
