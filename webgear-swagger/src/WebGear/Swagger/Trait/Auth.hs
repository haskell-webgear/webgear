{-# OPTIONS_GHC -Wno-orphans #-}

-- | Functions and instances for authentication
module WebGear.Swagger.Trait.Auth () where

import Data.Proxy (Proxy (..))
import Data.Swagger (
  Definitions,
  NamedSchema,
  Schema,
  ToSchema (..),
 )
import Data.Swagger.Declare (Declare)
import WebGear.Core.Trait.Auth.Common (AuthToken)

instance ToSchema (AuthToken scheme) where
  declareNamedSchema :: Proxy (AuthToken scheme) -> Declare (Definitions Schema) NamedSchema
  declareNamedSchema _ = declareNamedSchema $ Proxy @String
