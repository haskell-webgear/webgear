{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Status' trait.
module WebGear.OpenApi.Trait.Status where

import Control.Applicative ((<|>))
import Control.Lens (at, mapped, (%~), (&), (.~), (?~), (^.))
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.Maybe (fromMaybe)
import Data.OpenApi (
  Operation,
  PathItem,
  Referenced (..),
  Response,
  delete,
  description,
  get,
  head_,
  options,
  patch,
  paths,
  post,
  put,
  responses,
  trace,
 )
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Description (..))
import qualified WebGear.Core.Response as WG
import WebGear.Core.Trait (Set, With, setTrait)
import WebGear.Core.Trait.Status (Status (..))
import WebGear.OpenApi.Handler (OpenApiHandler (..), addRootPath, consumeDescription)

instance Set (OpenApiHandler m) Status where
  {-# INLINE setTrait #-}
  setTrait ::
    Status ->
    (WG.Response `With` ts -> WG.Response -> HTTP.Status -> WG.Response `With` (Status : ts)) ->
    OpenApiHandler m (WG.Response `With` ts, HTTP.Status) (WG.Response `With` (Status : ts))
  setTrait status _ = OpenApiHandler $ \doc -> do
    desc <- consumeDescription
    let doc' = if Map.null (doc ^. paths) then addRootPath doc else doc
    pure $ doc' & paths . mapped %~ setOperation desc status

setOperation :: Maybe Description -> Status -> PathItem -> PathItem
setOperation desc (Status status) item =
  item
    & delete %~ updateOperation
    & get %~ updateOperation
    & head_ %~ updateOperation
    & options %~ updateOperation
    & patch %~ updateOperation
    & post %~ updateOperation
    & put %~ updateOperation
    & trace %~ updateOperation
  where
    httpCode = HTTP.statusCode status

    updateOperation :: Maybe Operation -> Maybe Operation
    updateOperation Nothing = Just $ mempty @Operation & at httpCode ?~ addDescription emptyResp
    updateOperation (Just op) =
      let resp = addDescription $ fromMaybe emptyResp $ (op ^. at httpCode) <|> (op ^. at 0)
       in Just $ op & responses . responses %~ Map.insert httpCode resp . Map.delete 0

    emptyResp :: Referenced Response
    emptyResp = Inline mempty

    addDescription :: Referenced Response -> Referenced Response
    addDescription (Ref r) = Ref r
    addDescription (Inline r) =
      case desc of
        Nothing -> Inline r
        Just (Description d) -> Inline (r & description .~ d)
