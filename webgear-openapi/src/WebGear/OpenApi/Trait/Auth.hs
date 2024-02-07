{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Functions and instances for authentication
module WebGear.OpenApi.Trait.Auth () where

import Control.Lens ((?~))
import Data.Function ((&))
import Data.OpenApi (
  Definitions,
  HasType (type_),
  NamedSchema,
  Schema,
  ToParamSchema (..),
  ToSchema (..),
  paramSchemaToSchema,
 )
import Data.OpenApi.Declare (Declare)
import Data.OpenApi.Internal.Schema (plain)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol)
import WebGear.Core.Trait.Auth.Common (AuthToken)

instance (KnownSymbol scheme) => ToSchema (AuthToken scheme) where
  declareNamedSchema :: Proxy (AuthToken scheme) -> Declare (Definitions Schema) NamedSchema
  declareNamedSchema _ = declareNamedSchema $ Proxy @String
