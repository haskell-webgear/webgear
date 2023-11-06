{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'Status' trait.
module WebGear.Swagger.Trait.Status where

import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Set, With, setTrait)
import WebGear.Core.Trait.Status (Status (..))
import WebGear.Swagger.Handler (DocNode (DocStatus), SwaggerHandler (..), singletonNode)

instance Set (SwaggerHandler m) Status Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Status ->
    (Response `With` ts -> Response -> HTTP.Status -> Response `With` (Status : ts)) ->
    SwaggerHandler m (Response `With` ts, HTTP.Status) (Response `With` (Status : ts))
  setTrait (Status status) _ = SwaggerHandler $ singletonNode (DocStatus status)
