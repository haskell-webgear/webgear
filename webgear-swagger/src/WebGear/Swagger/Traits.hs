{- | Swagger implementation of all traits supported by WebGear.

 This modules only exports orphan instances imported from other
 modules. Hence the haddock documentation will be empty.
-}
module WebGear.Swagger.Traits () where

import WebGear.Swagger.Trait.Auth ()
import WebGear.Swagger.Trait.Auth.Basic ()
import WebGear.Swagger.Trait.Auth.JWT ()
import WebGear.Swagger.Trait.Body ()
import WebGear.Swagger.Trait.Cookie ()
import WebGear.Swagger.Trait.Header ()
import WebGear.Swagger.Trait.Method ()
import WebGear.Swagger.Trait.Path ()
import WebGear.Swagger.Trait.QueryParam ()
import WebGear.Swagger.Trait.Status ()
