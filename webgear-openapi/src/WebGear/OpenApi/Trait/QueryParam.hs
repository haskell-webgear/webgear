{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'QueryParam' trait.
module WebGear.OpenApi.Trait.QueryParam where

import Control.Lens ((&), (.~), (<>~))
import Control.Monad.State.Strict (MonadState)
import Data.OpenApi (
  OpenApi,
  Param (..),
  ParamLocation (ParamQuery),
  Referenced (Inline),
  ToSchema,
  allOperations,
  description,
  parameters,
  toSchema,
 )
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Handler (Description (..))
import WebGear.Core.Modifiers (Existence (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..))
import WebGear.Core.Trait.QueryParam (QueryParam (..))
import WebGear.OpenApi.Handler (Documentation (..), OpenApiHandler (..), consumeDescription)

instance (KnownSymbol name, ToSchema val) => Get (OpenApiHandler m) (QueryParam Required ps name val) Request where
  {-# INLINE getTrait #-}
  getTrait _ =
    let param =
          (mempty :: Param)
            { _paramName = fromString $ symbolVal $ Proxy @name
            , _paramIn = ParamQuery
            , _paramRequired = Just True
            , _paramSchema = Just $ Inline $ toSchema $ Proxy @val
            }
     in OpenApiHandler $ addParam param

instance (KnownSymbol name, ToSchema val) => Get (OpenApiHandler m) (QueryParam Optional ps name val) Request where
  {-# INLINE getTrait #-}
  getTrait _ =
    let param =
          (mempty :: Param)
            { _paramName = fromString $ symbolVal $ Proxy @name
            , _paramIn = ParamQuery
            , _paramRequired = Just False
            , _paramSchema = Just $ Inline $ toSchema $ Proxy @val
            }
     in OpenApiHandler $ addParam param

addParam :: (MonadState Documentation m) => Param -> OpenApi -> m OpenApi
addParam param doc = do
  desc <- consumeDescription
  let param' = param & description .~ fmap getDescription desc
  pure $ doc & allOperations . parameters <>~ [Inline param']
