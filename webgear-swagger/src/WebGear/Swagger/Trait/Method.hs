{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'Method' trait.
module WebGear.Swagger.Trait.Method where

import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..))
import WebGear.Core.Trait.Method (Method (..))
import WebGear.Swagger.Handler (DocNode (DocMethod), SwaggerHandler (SwaggerHandler), singletonNode)

instance Get (SwaggerHandler m) Method Request where
  {-# INLINE getTrait #-}
  getTrait (Method method) = SwaggerHandler $ singletonNode (DocMethod method)
