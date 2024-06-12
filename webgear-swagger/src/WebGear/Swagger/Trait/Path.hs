{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of path traits.
module WebGear.Swagger.Trait.Path where

import Control.Lens ((&), (<>~))
import Data.Data (Proxy (Proxy))
import Data.String (fromString)
import Data.Swagger (
  Param (..),
  ParamAnySchema (..),
  ParamLocation (ParamPath),
  ParamOtherSchema (..),
  Referenced (Inline),
  allOperations,
  parameters,
  prependPath,
 )
import Data.Text (unpack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), With)
import WebGear.Core.Trait.Path (Path (..), PathEnd (..), PathVar (..), PathVarError (..))
import WebGear.Swagger.Handler (SwaggerHandler (..), addRouteDocumentation)

instance Get (SwaggerHandler m) Path where
  {-# INLINE getTrait #-}
  getTrait :: Path -> SwaggerHandler m (Request `With` ts) (Either () ())
  getTrait (Path p) = SwaggerHandler $ addRouteDocumentation . prependPath (unpack p)

instance (KnownSymbol tag) => Get (SwaggerHandler m) (PathVar tag val) where
  {-# INLINE getTrait #-}
  getTrait :: PathVar tag val -> SwaggerHandler m (Request `With` ts) (Either PathVarError val)
  getTrait PathVar =
    let paramName = symbolVal $ Proxy @tag
        param =
          (mempty :: Param)
            { _paramName = fromString paramName
            , _paramRequired = Just True
            , _paramSchema =
                ParamOther
                  ParamOtherSchema
                    { _paramOtherSchemaIn = ParamPath
                    , _paramOtherSchemaParamSchema = mempty
                    , _paramOtherSchemaAllowEmptyValue = Nothing
                    }
            }
     in SwaggerHandler $ \doc ->
          addRouteDocumentation $
            prependPath ("{" <> paramName <> "}") doc
              & allOperations . parameters <>~ [Inline param]

instance Get (SwaggerHandler m) PathEnd where
  {-# INLINE getTrait #-}
  getTrait :: PathEnd -> SwaggerHandler m (Request `With` ts) (Either () ())
  getTrait PathEnd = SwaggerHandler $ addRouteDocumentation . prependPath "/"
