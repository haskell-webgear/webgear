{-# OPTIONS_GHC -Wno-orphans #-}

-- | Functions and instances for authentication
module WebGear.OpenApi.Trait.Auth (addSecurityScheme) where

import Control.Lens ((&), (.~), (<>~))
import Control.Monad.State.Strict (MonadState)
import Data.OpenApi (
  Definitions,
  NamedSchema,
  OpenApi,
  Schema,
  SecurityDefinitions (..),
  SecurityRequirement (..),
  SecurityScheme,
  ToSchema (..),
  allOperations,
  components,
  description,
  security,
  securitySchemes,
 )
import Data.OpenApi.Declare (Declare)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol)
import WebGear.Core.Handler (Description (..))
import WebGear.Core.Trait.Auth.Common (AuthToken)
import WebGear.OpenApi.Handler (Documentation (..), consumeDescription)

instance (KnownSymbol scheme) => ToSchema (AuthToken scheme) where
  declareNamedSchema :: Proxy (AuthToken scheme) -> Declare (Definitions Schema) NamedSchema
  declareNamedSchema _ = declareNamedSchema $ Proxy @String

addSecurityScheme :: (MonadState Documentation m) => Text -> SecurityScheme -> OpenApi -> m OpenApi
addSecurityScheme schemeName scheme doc = do
  desc <- consumeDescription
  let scheme' = scheme & description .~ fmap getDescription desc
      secSchemes = SecurityDefinitions [(schemeName, scheme')]
      secReqs = [SecurityRequirement [(schemeName, [])]] :: [SecurityRequirement]
  pure $
    doc
      & components . securitySchemes <>~ secSchemes
      & allOperations . security <>~ secReqs
