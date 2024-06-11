{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'Method' trait.
module WebGear.Swagger.Trait.Method where

import Control.Lens ((%~), (&))
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.Swagger (PathItem (..), paths)
import Network.HTTP.Types (StdMethod (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..))
import WebGear.Core.Trait.Method (Method (..))
import WebGear.Swagger.Handler (SwaggerHandler (..), addRouteDocumentation)

instance Get (SwaggerHandler m) Method Request where
  {-# INLINE getTrait #-}
  getTrait (Method method) = SwaggerHandler $ \doc -> do
    addRouteDocumentation $ doc & paths %~ Map.map (removeOtherMethods method)

removeOtherMethods :: StdMethod -> PathItem -> PathItem
removeOtherMethods method PathItem{..} =
  case method of
    GET -> mempty{_pathItemGet, _pathItemParameters}
    PUT -> mempty{_pathItemPut, _pathItemParameters}
    POST -> mempty{_pathItemPost, _pathItemParameters}
    DELETE -> mempty{_pathItemDelete, _pathItemParameters}
    HEAD -> mempty{_pathItemHead, _pathItemParameters}
    OPTIONS -> mempty{_pathItemOptions, _pathItemParameters}
    PATCH -> mempty{_pathItemPatch, _pathItemParameters}
    -- Swagger does not support CONNECT and TRACE
    CONNECT -> mempty{_pathItemParameters}
    TRACE -> mempty{_pathItemParameters}
