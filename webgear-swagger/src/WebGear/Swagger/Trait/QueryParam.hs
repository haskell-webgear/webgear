{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'QueryParam' trait.
module WebGear.Swagger.Trait.QueryParam where

import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Swagger (
  Param (..),
  ParamAnySchema (..),
  ParamLocation (ParamQuery),
  ParamOtherSchema (..),
 )
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Modifiers
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), TraitAbsence)
import WebGear.Core.Trait.QueryParam (QueryParam (..))
import WebGear.Swagger.Handler (DocNode (DocQueryParam), SwaggerHandler (..), singletonNode)

instance (KnownSymbol name, TraitAbsence (QueryParam Required ps name val) Request) => Get (SwaggerHandler m) (QueryParam Required ps name val) Request where
  {-# INLINE getTrait #-}
  getTrait _ =
    let param =
          (mempty :: Param)
            { _paramName = fromString $ symbolVal $ Proxy @name
            , _paramRequired = Just True
            , _paramSchema =
                ParamOther
                  $ ParamOtherSchema
                    { _paramOtherSchemaIn = ParamQuery
                    , _paramOtherSchemaAllowEmptyValue = Just True
                    , _paramOtherSchemaParamSchema = mempty
                    }
            }
     in SwaggerHandler $ singletonNode (DocQueryParam param)

instance (KnownSymbol name, TraitAbsence (QueryParam Optional ps name val) Request) => Get (SwaggerHandler m) (QueryParam Optional ps name val) Request where
  {-# INLINE getTrait #-}
  getTrait _ =
    let param =
          (mempty :: Param)
            { _paramName = fromString $ symbolVal $ Proxy @name
            , _paramRequired = Just False
            , _paramSchema =
                ParamOther
                  $ ParamOtherSchema
                    { _paramOtherSchemaIn = ParamQuery
                    , _paramOtherSchemaAllowEmptyValue = Just True
                    , _paramOtherSchemaParamSchema = mempty
                    }
            }
     in SwaggerHandler $ singletonNode (DocQueryParam param)
