{-# OPTIONS_GHC -Wno-orphans #-}

module WebGear.Server.Middleware.Method where

import Control.Arrow (returnA)
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Middleware.Method (Method (..), MethodMismatch (..))
import WebGear.Core.Request (Request, requestMethod)
import WebGear.Core.Trait (Get (..), Linked, unlink)
import WebGear.Server.Handler (ServerHandler)

instance Monad m => Get (ServerHandler m) Method Request where
  {-# INLINEABLE getTrait #-}
  getTrait :: Method -> ServerHandler m (Linked ts Request) (Either MethodMismatch HTTP.StdMethod)
  getTrait (Method method) = proc request -> do
    let expectedMethod = HTTP.renderStdMethod method
        actualMethod = requestMethod $ unlink request
    if actualMethod == expectedMethod
      then returnA -< Right method
      else returnA -< Left $ MethodMismatch{..}
