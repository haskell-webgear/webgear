{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'JWTAuth'' trait.
module WebGear.Swagger.Trait.Auth.JWT where

import Data.String (fromString)
import Data.Swagger
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Attribute, Get (..), TraitAbsence (..), With)
import WebGear.Core.Trait.Auth.JWT (JWTAuth' (..))
import WebGear.Swagger.Handler (SwaggerHandler (..))
import WebGear.Swagger.Trait.Auth (addSecurityScheme)

instance
  (TraitAbsence (JWTAuth' x scheme m e a) Request, KnownSymbol scheme) =>
  Get (SwaggerHandler m) (JWTAuth' x scheme m e a) Request
  where
  {-# INLINE getTrait #-}
  getTrait ::
    JWTAuth' x scheme m e a ->
    SwaggerHandler m (Request `With` ts) (Either (Absence (JWTAuth' x scheme m e a) Request) (Attribute (JWTAuth' x scheme m e a) Request))
  getTrait _ =
    let schemeName = fromString (symbolVal (Proxy @scheme))
        -- Swagger 2.0 does not support JWT: https://stackoverflow.com/a/32995636
        scheme =
          SecurityScheme
            { _securitySchemeType =
                SecuritySchemeApiKey
                  ( ApiKeyParams
                      { _apiKeyName = "JWT"
                      , _apiKeyIn = ApiKeyHeader
                      }
                  )
            , _securitySchemeDescription = Just ("Enter the token with the `" <> schemeName <> ": ` prefix")
            }
     in SwaggerHandler $ addSecurityScheme schemeName scheme
