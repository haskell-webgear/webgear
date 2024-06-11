{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'WG.Cookie' and 'WG.SetCookie' traits.
module WebGear.Swagger.Trait.Cookie () where

import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get (..), Set (..), Trait, TraitAbsence)
import qualified WebGear.Core.Trait.Cookie as WG
import WebGear.Swagger.Handler (SwaggerHandler (..))

-- Cookie information is not captured by Swagger

instance
  (TraitAbsence (WG.Cookie e name val) Request) =>
  Get (SwaggerHandler m) (WG.Cookie e name val) Request
  where
  {-# INLINE getTrait #-}
  getTrait WG.Cookie = SwaggerHandler pure

instance
  (Trait (WG.SetCookie e name) Response) =>
  Set (SwaggerHandler m) (WG.SetCookie e name) Response
  where
  {-# INLINE setTrait #-}
  setTrait WG.SetCookie _ = SwaggerHandler pure
