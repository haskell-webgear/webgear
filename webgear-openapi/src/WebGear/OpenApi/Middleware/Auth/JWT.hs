{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module WebGear.OpenApi.Middleware.Auth.JWT where

import Data.OpenApi
import Data.String (fromString)
import Data.Typeable (Proxy (..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Middleware.Auth.JWT (JWTAuth' (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Attribute, Get (..), Linked, TraitAbsence (..))
import WebGear.OpenApi.Handler (DocNode (DocSecurityScheme), OpenApiHandler (..), singletonNode)

instance (TraitAbsence (JWTAuth' x scheme m e a) Request, KnownSymbol scheme)
  => Get (OpenApiHandler m) (JWTAuth' x scheme m e a) Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: JWTAuth' x scheme m e a
           -> OpenApiHandler m (Linked ts Request) (Either (Absence (JWTAuth' x scheme m e a) Request) (Attribute (JWTAuth' x scheme m e a) Request))
  getTrait _ =
    let
      schemeName = "http" <> fromString (symbolVal (Proxy @scheme))
      securityScheme = SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp (HttpSchemeBearer (Just "JWT"))
        , _securitySchemeDescription = Nothing
        }
    in
      OpenApiHandler $ singletonNode (DocSecurityScheme schemeName securityScheme)
