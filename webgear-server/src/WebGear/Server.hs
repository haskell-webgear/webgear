{- | Main module for WebGear server.

 One could import this module to get all required types and
 functions to build a WebGear server.
-}
module WebGear.Server (
  module WebGear.Core,
  module WebGear.Server.Handler,
) where

import WebGear.Core
import WebGear.Server.Handler
import WebGear.Server.Traits ()
