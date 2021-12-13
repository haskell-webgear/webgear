{- | Main module for WebGear server.

 Import this module to get all required types and functions to build a
 WebGear server. Alternatively, import individual modules under
 @WebGear.Server@.

 Typical usage to implement a server is:

@
 import WebGear.Server
 import Network.Wai (Application)
 import qualified Network.Wai.Handler.Warp as Warp

 -- | A monad stack for you application
 type App a = ....

 -- | Convert the App monad to IO
 runApp :: App a -> IO a
 runApp = ....

 -- | Handler for the server
 myHandler :: `Handler` h App => `RequestHandler` h '[]
 myHandler = ....

 app :: Application
 app = `toApplication` (`transform` runApp myHandler)

 main :: IO ()
 main = Warp.run port app
@
-}
module WebGear.Server (
  module WebGear.Core,
  module WebGear.Server.Handler,
) where

import WebGear.Core
import WebGear.Server.Handler
import WebGear.Server.Traits ()
