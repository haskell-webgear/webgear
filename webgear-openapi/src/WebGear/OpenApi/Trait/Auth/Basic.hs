{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of `BasicAuth'` trait.
module WebGear.OpenApi.Trait.Auth.Basic where

import Data.OpenApi
import Data.Proxy (Proxy (..))
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Attribute, Absence, Get (..), With)
import WebGear.Core.Trait.Auth.Basic (BasicAuth' (..))
import WebGear.OpenApi.Handler (OpenApiHandler (..))
import WebGear.OpenApi.Trait.Auth (addSecurityScheme)

instance (KnownSymbol scheme) => Get (OpenApiHandler m) (BasicAuth' x scheme m e a) Request where
  {-# INLINE getTrait #-}
  getTrait ::
    BasicAuth' x scheme m e a ->
    OpenApiHandler m (Request `With` ts) (Either (Absence (BasicAuth' x scheme m e a) Request) (Attribute (BasicAuth' x scheme m e a) Request))
  getTrait _ =
    let schemeName = "http" <> fromString (symbolVal (Proxy @scheme))
        scheme =
          SecurityScheme
            { _securitySchemeType = SecuritySchemeHttp HttpSchemeBasic
            , _securitySchemeDescription = Nothing
            }
     in OpenApiHandler $ addSecurityScheme schemeName scheme
