{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of path traits.
module WebGear.OpenApi.Trait.Path where

import Data.Data (Proxy (Proxy))
import Data.OpenApi (Param (..), ParamLocation (ParamPath), Referenced (Inline), ToSchema, toSchema)
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), Linked)
import WebGear.Core.Trait.Path (Path (..), PathEnd (..), PathVar (..), PathVarError (..))
import WebGear.OpenApi.Handler (
  DocNode (DocPathElem, DocPathVar),
  OpenApiHandler (..),
  singletonNode,
 )

instance Get (OpenApiHandler m) Path Request where
  {-# INLINE getTrait #-}
  getTrait :: Path -> OpenApiHandler m (Linked ts Request) (Either () ())
  getTrait (Path p) = OpenApiHandler $ singletonNode (DocPathElem p)

instance (KnownSymbol tag, ToSchema val) => Get (OpenApiHandler m) (PathVar tag val) Request where
  {-# INLINE getTrait #-}
  getTrait :: PathVar tag val -> OpenApiHandler m (Linked ts Request) (Either PathVarError val)
  getTrait PathVar =
    let param =
          (mempty :: Param)
            { _paramName = fromString $ symbolVal $ Proxy @tag
            , _paramIn = ParamPath
            , _paramRequired = Just True
            , _paramSchema = Just $ Inline $ toSchema $ Proxy @val
            }
     in OpenApiHandler $ singletonNode (DocPathVar param)

instance Get (OpenApiHandler m) PathEnd Request where
  {-# INLINE getTrait #-}
  getTrait :: PathEnd -> OpenApiHandler m (Linked ts Request) (Either () ())
  getTrait PathEnd = OpenApiHandler $ singletonNode (DocPathElem "/")
