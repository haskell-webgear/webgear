{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Method' trait.
module WebGear.OpenApi.Trait.Method where

import WebGear.Core.Middleware.Method (Method (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..))
import WebGear.OpenApi.Handler (DocNode (DocMethod), OpenApiHandler (OpenApiHandler), singletonNode)

instance Get (OpenApiHandler m) Method Request where
  {-# INLINEABLE getTrait #-}
  getTrait (Method method) = OpenApiHandler $ singletonNode (DocMethod method)
