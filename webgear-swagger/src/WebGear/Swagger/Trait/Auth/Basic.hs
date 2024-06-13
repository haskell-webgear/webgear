{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of `BasicAuth'` trait.
module WebGear.Swagger.Trait.Auth.Basic where

import Data.Proxy (Proxy (..))
import Data.String (fromString)
import Data.Swagger
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Absence, Attribute, Get (..), With)
import WebGear.Core.Trait.Auth.Basic (BasicAuth' (..))
import WebGear.Swagger.Handler (SwaggerHandler (..))
import WebGear.Swagger.Trait.Auth (addSecurityScheme)

instance (KnownSymbol scheme) => Get (SwaggerHandler m) (BasicAuth' x scheme m e a) where
  {-# INLINE getTrait #-}
  getTrait ::
    BasicAuth' x scheme m e a ->
    SwaggerHandler m (Request `With` ts) (Either (Absence (BasicAuth' x scheme m e a)) (Attribute (BasicAuth' x scheme m e a) Request))
  getTrait _ =
    let schemeName = "http" <> fromString (symbolVal (Proxy @scheme))
        scheme =
          SecurityScheme
            { _securitySchemeType = SecuritySchemeBasic
            , _securitySchemeDescription = Nothing
            }
     in SwaggerHandler $ addSecurityScheme schemeName scheme
