{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Status' trait.
module WebGear.OpenApi.Trait.Status where

import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Linked, Set, setTrait)
import WebGear.Core.Trait.Status (Status (..))
import WebGear.OpenApi.Handler (DocNode (DocStatus), OpenApiHandler (..), singletonNode)

instance Set (OpenApiHandler m) Status Response where
  {-# INLINEABLE setTrait #-}
  setTrait ::
    Status ->
    (Linked ts Response -> Response -> HTTP.Status -> Linked (Status : ts) Response) ->
    OpenApiHandler m (Linked ts Response, HTTP.Status) (Linked (Status : ts) Response)
  setTrait (Status status doc) _ = OpenApiHandler $ singletonNode (DocStatus status doc)
