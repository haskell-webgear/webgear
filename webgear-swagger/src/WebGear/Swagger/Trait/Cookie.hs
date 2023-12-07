{-# OPTIONS_GHC -Wno-orphans #-}

{- | Swagger implementation of 'Cookie' and 'SetCookie' traits.

Swagger 2.0 does not support cookie authentication. So these
implementations are no-ops.
-}
module WebGear.Swagger.Trait.Cookie () where

import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get (..), Set (..), Trait, TraitAbsence)
import qualified WebGear.Core.Trait.Cookie as WG
import WebGear.Swagger.Handler (SwaggerHandler (..), nullNode)

instance
  (TraitAbsence (WG.Cookie e name val) Request) =>
  Get (SwaggerHandler m) (WG.Cookie e name val) Request
  where
  {-# INLINE getTrait #-}
  getTrait WG.Cookie = SwaggerHandler nullNode

instance
  (Trait (WG.SetCookie e name) Response) =>
  Set (SwaggerHandler m) (WG.SetCookie e name) Response
  where
  {-# INLINE setTrait #-}
  setTrait WG.SetCookie _ = SwaggerHandler nullNode
