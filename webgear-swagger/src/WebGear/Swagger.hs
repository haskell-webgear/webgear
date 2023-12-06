{- | Main module for WebGear Swagger support.

 Import this module to get all required types and functions for
 generating Swagget specifications. Alternatively, import individual
 modules under @WebGear.Swagger@.

 Typical usage to generate Swagger specifications:

@
 import WebGear.Swagger
 import Data.Swagger (Swagger)

 myHandler :: Handler h m => RequestHandler h '[]
 myHandler = ....

 documentation :: Swagger
 documentation = toSwagger myHandler
@
-}
module WebGear.Swagger (
  module WebGear.Core,
  module WebGear.Swagger.Handler,
) where

import WebGear.Core
import WebGear.Swagger.Handler
import WebGear.Swagger.Traits ()
