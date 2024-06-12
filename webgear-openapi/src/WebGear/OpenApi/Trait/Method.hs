{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Method' trait.
module WebGear.OpenApi.Trait.Method where

import Control.Lens ((%~), (&))
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.OpenApi (PathItem (..), paths)
import Network.HTTP.Types (StdMethod (..))
import WebGear.Core.Trait (Get (..))
import WebGear.Core.Trait.Method (Method (..))
import WebGear.OpenApi.Handler (OpenApiHandler (..), addRouteDocumentation)

instance Get (OpenApiHandler m) Method where
  {-# INLINE getTrait #-}
  getTrait (Method method) = OpenApiHandler $ \doc -> do
    addRouteDocumentation $ doc & paths %~ Map.map (removeOtherMethods method)

removeOtherMethods :: StdMethod -> PathItem -> PathItem
removeOtherMethods method PathItem{..} =
  case method of
    GET -> mempty{_pathItemGet, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    PUT -> mempty{_pathItemPut, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    POST -> mempty{_pathItemPost, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    DELETE -> mempty{_pathItemDelete, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    HEAD -> mempty{_pathItemHead, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    TRACE -> mempty{_pathItemTrace, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    OPTIONS -> mempty{_pathItemOptions, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    PATCH -> mempty{_pathItemPatch, _pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
    -- OpenApi does not support CONNECT
    CONNECT -> mempty{_pathItemSummary, _pathItemDescription, _pathItemServers, _pathItemParameters}
