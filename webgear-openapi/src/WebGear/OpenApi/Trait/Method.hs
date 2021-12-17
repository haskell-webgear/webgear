{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Method' trait.
module WebGear.OpenApi.Trait.Method where

import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..))
import WebGear.Core.Trait.Method (Method (..))
import WebGear.OpenApi.Handler (DocNode (DocMethod), OpenApiHandler (OpenApiHandler), singletonNode)

instance Get (OpenApiHandler m) Method Request where
  {-# INLINEABLE getTrait #-}
  getTrait (Method method doc) = OpenApiHandler $ singletonNode (DocMethod method doc)
