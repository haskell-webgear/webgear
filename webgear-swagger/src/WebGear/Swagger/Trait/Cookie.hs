{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'WG.Cookie' and 'WG.SetCookie' traits.
module WebGear.Swagger.Trait.Cookie () where

import WebGear.Core.Trait (Get (..), Set (..))
import qualified WebGear.Core.Trait.Cookie as WG
import WebGear.Swagger.Handler (SwaggerHandler (..))

-- Cookie information is not captured by Swagger

instance Get (SwaggerHandler m) (WG.Cookie e name val) where
  {-# INLINE getTrait #-}
  getTrait WG.Cookie = SwaggerHandler pure

instance Set (SwaggerHandler m) (WG.SetCookie e name) where
  {-# INLINE setTrait #-}
  setTrait WG.SetCookie _ = SwaggerHandler pure
