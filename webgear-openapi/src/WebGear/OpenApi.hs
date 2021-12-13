{- | Main module for WebGear OpenApi handler.

 This module can be imported to get all required types and functions
 to generate OpenApi documentation.
-}
module WebGear.OpenApi (
  module WebGear.Core,
  module WebGear.OpenApi.Handler,
) where

import WebGear.Core
import WebGear.OpenApi.Handler
import WebGear.OpenApi.Traits ()
