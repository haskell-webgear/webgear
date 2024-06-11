{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'JWTAuth'' trait.
module WebGear.OpenApi.Trait.Auth.JWT where

import Data.OpenApi
import Data.String (fromString)
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Attribute, Absence, Get (..), With)
import WebGear.Core.Trait.Auth.JWT (JWTAuth' (..))
import WebGear.OpenApi.Handler (OpenApiHandler (..))
import WebGear.OpenApi.Trait.Auth (addSecurityScheme)

instance
  (KnownSymbol scheme) =>
  Get (OpenApiHandler m) (JWTAuth' x scheme m e a) Request
  where
  {-# INLINE getTrait #-}
  getTrait ::
    JWTAuth' x scheme m e a ->
    OpenApiHandler m (Request `With` ts) (Either (Absence (JWTAuth' x scheme m e a) Request) (Attribute (JWTAuth' x scheme m e a) Request))
  getTrait _ =
    let schemeName = "http" <> fromString (symbolVal (Proxy @scheme))
        scheme =
          SecurityScheme
            { _securitySchemeType = SecuritySchemeHttp (HttpSchemeBearer (Just "JWT"))
            , _securitySchemeDescription = Nothing
            }
     in OpenApiHandler $ addSecurityScheme schemeName scheme
