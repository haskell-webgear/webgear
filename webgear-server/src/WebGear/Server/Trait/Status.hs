{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the `Status` trait.
module WebGear.Server.Trait.Status where

import Control.Arrow (returnA)
import qualified Network.HTTP.Types.Status as HTTP
import WebGear.Core.Response (Response (responseStatus))
import WebGear.Core.Trait (Set, With, setTrait, unwitness)
import WebGear.Core.Trait.Status (Status (..))
import WebGear.Server.Handler (ServerHandler)

instance (Monad m) => Set (ServerHandler m) Status Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Status ->
    (Response `With` ts -> Response -> HTTP.Status -> Response `With` (Status : ts)) ->
    ServerHandler m (Response `With` ts, HTTP.Status) (Response `With` (Status : ts))
  setTrait (Status status) f = proc (response, _) -> do
    let response' = (unwitness response){responseStatus = status}
    returnA -< f response response' status
