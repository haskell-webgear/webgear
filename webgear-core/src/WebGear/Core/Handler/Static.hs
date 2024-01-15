{- |
 Handlers for serving static resources
-}
module WebGear.Core.Handler.Static (
  serveStatic,
) where

import Control.Arrow (returnA)
import Network.Wai (Request (..))
import qualified Network.Wai.Application.Static as Wai.Static
import WebGear.Core.Handler (Handler (..), RequestHandler, RoutePath (..))
import WebGear.Core.Request (toWaiRequest)
import WebGear.Core.Response (Response (ResponseCont))
import WebGear.Core.Trait (unwitness)
import Prelude hiding (readFile)

-- | Serve static assets
serveStatic :: (Handler h m) => Wai.Static.StaticSettings -> RequestHandler h ts
serveStatic settings =
  proc request -> do
    RoutePath pathInfo <- consumeRoute returnA -< ()
    let waiRequest = toWaiRequest $ unwitness request
    returnA -< ResponseCont $ Wai.Static.staticApp settings waiRequest{pathInfo}
