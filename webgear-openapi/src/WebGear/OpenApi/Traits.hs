{- | OpenAPI implementation of all traits supported by WebGear.

 This modules only exports orphan instances imported from other
 modules. Hence the haddock documentation will be empty.
-}
module WebGear.OpenApi.Traits () where

import WebGear.OpenApi.Trait.Auth.Basic ()
import WebGear.OpenApi.Trait.Auth.JWT ()
import WebGear.OpenApi.Trait.Body ()
import WebGear.OpenApi.Trait.Header ()
import WebGear.OpenApi.Trait.Method ()
import WebGear.OpenApi.Trait.Path ()
import WebGear.OpenApi.Trait.QueryParam ()
import WebGear.OpenApi.Trait.Status ()
