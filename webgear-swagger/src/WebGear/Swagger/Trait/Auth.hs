{-# OPTIONS_GHC -Wno-orphans #-}

-- | Functions and instances for authentication
module WebGear.Swagger.Trait.Auth (addSecurityScheme) where

import Control.Lens ((&), (.~), (<>~))
import Control.Monad.State.Strict (MonadState)
import Data.Proxy (Proxy (..))
import Data.Swagger (
  Definitions,
  NamedSchema,
  Schema,
  SecurityDefinitions (..),
  SecurityRequirement (..),
  SecurityScheme,
  Swagger,
  ToSchema (..),
  allOperations,
  description,
  security,
  securityDefinitions,
 )
import Data.Swagger.Declare (Declare)
import Data.Text (Text)
import WebGear.Core.Handler (Description (..))
import WebGear.Core.Trait.Auth.Common (AuthToken)
import WebGear.Swagger.Handler (Documentation (..), consumeDescription)

instance ToSchema (AuthToken scheme) where
  declareNamedSchema :: Proxy (AuthToken scheme) -> Declare (Definitions Schema) NamedSchema
  declareNamedSchema _ = declareNamedSchema $ Proxy @String

addSecurityScheme :: (MonadState Documentation m) => Text -> SecurityScheme -> Swagger -> m Swagger
addSecurityScheme schemeName scheme doc = do
  desc <- consumeDescription
  let scheme' = scheme & description .~ fmap getDescription desc
      secDefs = SecurityDefinitions [(schemeName, scheme')]
      secReqs = [SecurityRequirement [(schemeName, [])]] :: [SecurityRequirement]
  pure $
    doc
      & securityDefinitions <>~ secDefs
      & allOperations . security <>~ secReqs
