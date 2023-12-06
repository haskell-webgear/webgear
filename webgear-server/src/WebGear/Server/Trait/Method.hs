{-# OPTIONS_GHC -Wno-orphans #-}

-- | Server implementation of the `Method` trait.
module WebGear.Server.Trait.Method where

import Control.Arrow (returnA)
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Request (Request, requestMethod)
import WebGear.Core.Trait (Get (..), With (unwitness), unwitness)
import WebGear.Core.Trait.Method (Method (..), MethodMismatch (..))
import WebGear.Server.Handler (ServerHandler)

instance (Monad m) => Get (ServerHandler m) Method Request where
  {-# INLINE getTrait #-}
  getTrait :: Method -> ServerHandler m (Request `With` ts) (Either MethodMismatch HTTP.StdMethod)
  getTrait (Method method) = proc request -> do
    let expectedMethod = HTTP.renderStdMethod method
        actualMethod = requestMethod $ unwitness request
    if actualMethod == expectedMethod
      then returnA -< Right method
      else returnA -< Left $ MethodMismatch{..}
