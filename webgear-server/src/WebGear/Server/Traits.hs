{- | Server implementation of all traits supported by WebGear.

 This modules only exports orphan instances imported from other
 modules. Hence the haddock documentation will be empty.
-}
module WebGear.Server.Traits () where

import WebGear.Server.Trait.Auth.Basic ()
import WebGear.Server.Trait.Auth.JWT ()
import WebGear.Server.Trait.Body ()
import WebGear.Server.Trait.Cookie ()
import WebGear.Server.Trait.Header ()
import WebGear.Server.Trait.Method ()
import WebGear.Server.Trait.Path ()
import WebGear.Server.Trait.QueryParam ()
import WebGear.Server.Trait.Status ()
