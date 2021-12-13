{- | Main module for WebGear OpenAPI support.

 Import this module to get all required types and functions for
 generating OpenAPI documentation. Alternatively, import individual
 modules under @WebGear.OpenApi@.

 Typical usage to generate OpenAPI:

@
 import WebGear.OpenApi
 import Data.OpenApi (OpenApi)

 myHandler :: Handler h m => RequestHandler h '[]
 myHandler = ....

 documentation :: OpenApi
 documentation = toOpenApi myHandler
@
-}
module WebGear.OpenApi (
  module WebGear.Core,
  module WebGear.OpenApi.Handler,
) where

import WebGear.Core
import WebGear.OpenApi.Handler
import WebGear.OpenApi.Traits ()
