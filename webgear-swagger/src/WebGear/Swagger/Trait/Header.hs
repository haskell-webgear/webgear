{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Header' trait.
module WebGear.OpenApi.Trait.Header () where

import Control.Lens ((&), (.~), (?~))
import Data.OpenApi hiding (Response)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Modifiers (Existence (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get (..), Set (..), TraitAbsence)
import qualified WebGear.Core.Trait.Header as WG
import WebGear.OpenApi.Handler (DocNode (..), OpenApiHandler (..), nullNode, singletonNode)

mkParam ::
  forall name val.
  (KnownSymbol name, ToSchema val) =>
  Proxy name ->
  Proxy val ->
  Bool ->
  Param
mkParam _ _ isRequired =
  (mempty :: Param)
    & name .~ fromString @Text (symbolVal $ Proxy @name)
    & in_ .~ ParamHeader
    & required ?~ isRequired
    & schema ?~ Inline (toSchema $ Proxy @val)

instance (KnownSymbol name, ToSchema val, TraitAbsence (WG.RequestHeader Required ps name val) Request) => Get (OpenApiHandler m) (WG.RequestHeader Required ps name val) Request where
  {-# INLINE getTrait #-}
  getTrait WG.RequestHeader =
    OpenApiHandler $ singletonNode (DocRequestHeader $ mkParam (Proxy @name) (Proxy @val) True)

instance (KnownSymbol name, ToSchema val, TraitAbsence (WG.RequestHeader Optional ps name val) Request) => Get (OpenApiHandler m) (WG.RequestHeader Optional ps name val) Request where
  {-# INLINE getTrait #-}
  getTrait WG.RequestHeader =
    OpenApiHandler $ singletonNode (DocRequestHeader $ mkParam (Proxy @name) (Proxy @val) False)

instance (KnownSymbol name, ToSchema val) => Set (OpenApiHandler m) (WG.ResponseHeader Required name val) Response where
  {-# INLINE setTrait #-}
  setTrait WG.ResponseHeader _ =
    let headerName = fromString $ symbolVal $ Proxy @name
        header =
          mempty @Header
            & required ?~ True
            & schema ?~ Inline (toSchema $ Proxy @val)
     in if headerName == "Content-Type"
          then OpenApiHandler nullNode
          else OpenApiHandler $ singletonNode (DocResponseHeader headerName header)

instance (KnownSymbol name, ToSchema val) => Set (OpenApiHandler m) (WG.ResponseHeader Optional name val) Response where
  {-# INLINE setTrait #-}
  setTrait WG.ResponseHeader _ =
    let headerName = fromString $ symbolVal $ Proxy @name
        header =
          mempty @Header
            & required ?~ True
            & schema ?~ Inline (toSchema $ Proxy @val)
     in OpenApiHandler $ singletonNode (DocResponseHeader headerName header)
