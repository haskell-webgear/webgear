{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'QueryParam' trait.
module WebGear.Swagger.Trait.QueryParam where

import Control.Lens ((&), (.~), (<>~))
import Control.Monad.State.Strict (MonadState)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Swagger (
  Param (..),
  ParamAnySchema (..),
  ParamLocation (ParamQuery),
  ParamOtherSchema (..),
  Referenced (Inline),
  Swagger,
  allOperations,
  description,
  parameters,
 )
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Handler (Description (..))
import WebGear.Core.Modifiers (Existence (..))
import WebGear.Core.Trait (Get (..))
import WebGear.Core.Trait.QueryParam (QueryParam (..))
import WebGear.Swagger.Handler (Documentation (..), SwaggerHandler (..), consumeDescription)

instance (KnownSymbol name) => Get (SwaggerHandler m) (QueryParam Required ps name val) where
  {-# INLINE getTrait #-}
  getTrait _ =
    let param =
          (mempty :: Param)
            { _paramName = fromString $ symbolVal $ Proxy @name
            , _paramRequired = Just True
            , _paramSchema =
                ParamOther
                  ParamOtherSchema
                    { _paramOtherSchemaIn = ParamQuery
                    , _paramOtherSchemaAllowEmptyValue = Just True
                    , _paramOtherSchemaParamSchema = mempty
                    }
            }
     in SwaggerHandler $ addParam param

instance (KnownSymbol name) => Get (SwaggerHandler m) (QueryParam Optional ps name val) where
  {-# INLINE getTrait #-}
  getTrait _ =
    let param =
          (mempty :: Param)
            { _paramName = fromString $ symbolVal $ Proxy @name
            , _paramRequired = Just False
            , _paramSchema =
                ParamOther
                  ParamOtherSchema
                    { _paramOtherSchemaIn = ParamQuery
                    , _paramOtherSchemaAllowEmptyValue = Just True
                    , _paramOtherSchemaParamSchema = mempty
                    }
            }
     in SwaggerHandler $ addParam param

addParam :: (MonadState Documentation m) => Param -> Swagger -> m Swagger
addParam param doc = do
  desc <- consumeDescription
  let param' = param & description .~ fmap getDescription desc
  pure $ doc & allOperations . parameters <>~ [Inline param']
