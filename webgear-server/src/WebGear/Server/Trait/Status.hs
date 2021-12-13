{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the `Status` trait.
module WebGear.Server.Trait.Status where

import Control.Arrow (returnA)
import qualified Network.HTTP.Types.Status as HTTP
import WebGear.Core.Middleware.Status (Status (..))
import WebGear.Core.Response (Response (responseStatus))
import WebGear.Core.Trait (Linked, Set, setTrait, unlink)
import WebGear.Server.Handler (ServerHandler)

instance Monad m => Set (ServerHandler m) Status Response where
  {-# INLINEABLE setTrait #-}
  setTrait ::
    Status ->
    (Linked ts Response -> Response -> HTTP.Status -> Linked (Status : ts) Response) ->
    ServerHandler m (Linked ts Response, HTTP.Status) (Linked (Status : ts) Response)
  setTrait (Status status) f = proc (linkedResponse, _) -> do
    let response = unlink linkedResponse
        response' = response{responseStatus = status}
    returnA -< f linkedResponse response' status
