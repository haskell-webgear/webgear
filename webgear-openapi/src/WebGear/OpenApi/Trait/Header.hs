{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Header' trait.
module WebGear.OpenApi.Trait.Header () where

import Control.Lens ((&), (.~), (?~))
import Data.OpenApi hiding (Response)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Modifiers (Documentation (..), Existence (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get (..), Set (..), Trait, TraitAbsence)
import qualified WebGear.Core.Trait.Header as WG
import WebGear.OpenApi.Handler (DocNode (..), OpenApiHandler (..), singletonNode)

mkParam ::
  forall name val.
  (KnownSymbol name, ToSchema val) =>
  Proxy name ->
  Proxy val ->
  Bool ->
  Documentation ->
  Param
mkParam _ _ isRequired Documentation{docDescription} =
  (mempty :: Param)
    & name .~ fromString @Text (symbolVal $ Proxy @name)
    & in_ .~ ParamHeader
    & required ?~ isRequired
    & description .~ docDescription
    & schema ?~ Inline (toSchema $ Proxy @val)

instance (KnownSymbol name, ToSchema val, TraitAbsence (WG.Header Required ps name val) Request) => Get (OpenApiHandler m) (WG.Header Required ps name val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait (WG.Header doc) =
    OpenApiHandler $ singletonNode (DocRequestHeader $ mkParam (Proxy @name) (Proxy @val) True doc)

instance (KnownSymbol name, ToSchema val, TraitAbsence (WG.Header Optional ps name val) Request) => Get (OpenApiHandler m) (WG.Header Optional ps name val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait (WG.Header doc) =
    OpenApiHandler $ singletonNode (DocRequestHeader $ mkParam (Proxy @name) (Proxy @val) False doc)

instance (KnownSymbol name, ToSchema val, Trait (WG.Header Required ps name val) Response) => Set (OpenApiHandler m) (WG.Header Required ps name val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait (WG.Header Documentation{docDescription}) _ =
    let headerName = fromString $ symbolVal $ Proxy @name
        header =
          mempty @Header
            & required ?~ True
            & description .~ docDescription
            & schema ?~ Inline (toSchema $ Proxy @val)
     in OpenApiHandler $ singletonNode (DocResponseHeader headerName header)

instance (KnownSymbol name, ToSchema val, Trait (WG.Header Optional ps name val) Response) => Set (OpenApiHandler m) (WG.Header Optional ps name val) Response where
  {-# INLINEABLE setTrait #-}
  setTrait (WG.Header Documentation{docDescription}) _ =
    let headerName = fromString $ symbolVal $ Proxy @name
        header =
          mempty @Header
            & required ?~ True
            & description .~ docDescription
            & schema ?~ Inline (toSchema $ Proxy @val)
     in OpenApiHandler $ singletonNode (DocResponseHeader headerName header)
