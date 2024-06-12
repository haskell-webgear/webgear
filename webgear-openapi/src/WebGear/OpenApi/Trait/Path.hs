{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of path traits.
module WebGear.OpenApi.Trait.Path where

import Control.Lens ((&), (<>~))
import Data.Data (Proxy (Proxy))
import Data.OpenApi (
  Param (..),
  ParamLocation (ParamPath),
  Referenced (Inline),
  ToSchema,
  allOperations,
  parameters,
  prependPath,
  toSchema,
 )
import Data.String (fromString)
import Data.Text (unpack)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), With)
import WebGear.Core.Trait.Path (Path (..), PathEnd (..), PathVar (..), PathVarError (..))
import WebGear.OpenApi.Handler (OpenApiHandler (..), addRouteDocumentation)

instance Get (OpenApiHandler m) Path where
  {-# INLINE getTrait #-}
  getTrait :: Path -> OpenApiHandler m (Request `With` ts) (Either () ())
  getTrait (Path p) = OpenApiHandler $ addRouteDocumentation . prependPath (unpack p)

instance (KnownSymbol tag, ToSchema val) => Get (OpenApiHandler m) (PathVar tag val) where
  {-# INLINE getTrait #-}
  getTrait :: PathVar tag val -> OpenApiHandler m (Request `With` ts) (Either PathVarError val)
  getTrait PathVar =
    let paramName = symbolVal $ Proxy @tag
        param =
          (mempty :: Param)
            { _paramName = fromString paramName
            , _paramIn = ParamPath
            , _paramRequired = Just True
            , _paramSchema = Just $ Inline $ toSchema $ Proxy @val
            }
     in OpenApiHandler $ \doc ->
          addRouteDocumentation $
            prependPath ("{" <> paramName <> "}") doc
              & allOperations . parameters <>~ [Inline param]

instance Get (OpenApiHandler m) PathEnd where
  {-# INLINE getTrait #-}
  getTrait :: PathEnd -> OpenApiHandler m (Request `With` ts) (Either () ())
  getTrait PathEnd = OpenApiHandler $ addRouteDocumentation . prependPath "/"
