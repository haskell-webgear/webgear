{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Status' trait.
module WebGear.OpenApi.Trait.Status where

import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Set, With, setTrait)
import WebGear.Core.Trait.Status (Status (..))
import WebGear.OpenApi.Handler (DocNode (DocStatus), OpenApiHandler (..), singletonNode)

instance Set (OpenApiHandler m) Status Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Status ->
    (Response `With` ts -> Response -> HTTP.Status -> Response `With` (Status : ts)) ->
    OpenApiHandler m (Response `With` ts, HTTP.Status) (Response `With` (Status : ts))
  setTrait (Status status) _ = OpenApiHandler $ singletonNode (DocStatus status)
