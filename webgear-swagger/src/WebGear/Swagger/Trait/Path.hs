{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of path traits.
module WebGear.Swagger.Trait.Path where

import Data.Data (Proxy (Proxy))
import Data.String (fromString)
import Data.Swagger (
  Param (..),
  ParamAnySchema (ParamOther),
  ParamLocation (ParamPath),
  ParamOtherSchema (..),
 )
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), With)
import WebGear.Core.Trait.Path (Path (..), PathEnd (..), PathVar (..), PathVarError (..))
import WebGear.Swagger.Handler (
  DocNode (DocPathElem, DocPathVar),
  SwaggerHandler (..),
  singletonNode,
 )

instance Get (SwaggerHandler m) Path Request where
  {-# INLINE getTrait #-}
  getTrait :: Path -> SwaggerHandler m (Request `With` ts) (Either () ())
  getTrait (Path p) = SwaggerHandler $ singletonNode (DocPathElem p)

instance (KnownSymbol tag) => Get (SwaggerHandler m) (PathVar tag val) Request where
  {-# INLINE getTrait #-}
  getTrait :: PathVar tag val -> SwaggerHandler m (Request `With` ts) (Either PathVarError val)
  getTrait PathVar =
    let param =
          (mempty :: Param)
            { _paramName = fromString $ symbolVal $ Proxy @tag
            , _paramRequired = Just True
            , _paramSchema =
                ParamOther
                  $ ParamOtherSchema
                    { _paramOtherSchemaIn = ParamPath
                    , _paramOtherSchemaParamSchema = mempty
                    , _paramOtherSchemaAllowEmptyValue = Nothing
                    }
            }
     in SwaggerHandler $ singletonNode (DocPathVar param)

instance Get (SwaggerHandler m) PathEnd Request where
  {-# INLINE getTrait #-}
  getTrait :: PathEnd -> SwaggerHandler m (Request `With` ts) (Either () ())
  getTrait PathEnd = SwaggerHandler $ singletonNode (DocPathElem "/")
