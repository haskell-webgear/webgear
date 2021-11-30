{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
module WebGear.OpenApi.Middleware.QueryParam where

import Data.OpenApi (Param (..), ParamLocation (ParamQuery), Referenced (Inline), ToSchema,
                     toSchema)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Middleware.QueryParam (QueryParam (..))
import WebGear.Core.Modifiers
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), TraitAbsence)
import WebGear.OpenApi.Handler (DocNode (DocQueryParam), OpenApiHandler (..), singletonNode)


instance (KnownSymbol name, ToSchema val, TraitAbsence (QueryParam Required ps name val) Request) => Get (OpenApiHandler m) (QueryParam Required ps name val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait QueryParam =
    let
      param = (mempty :: Param)
              { _paramName = fromString $ symbolVal $ Proxy @name
              , _paramIn = ParamQuery
              , _paramRequired = Just True
              , _paramSchema = Just $ Inline $ toSchema $ Proxy @val
              }
    in
      OpenApiHandler $ singletonNode (DocQueryParam param)

instance (KnownSymbol name, ToSchema val, TraitAbsence (QueryParam Optional ps name val) Request) => Get (OpenApiHandler m) (QueryParam Optional ps name val) Request where
  {-# INLINEABLE getTrait #-}
  getTrait QueryParam =
    let
      param = (mempty :: Param)
              { _paramName = fromString $ symbolVal $ Proxy @name
              , _paramIn = ParamQuery
              , _paramRequired = Just False
              , _paramSchema = Just $ Inline $ toSchema $ Proxy @val
              }
    in
      OpenApiHandler $ singletonNode (DocQueryParam param)
