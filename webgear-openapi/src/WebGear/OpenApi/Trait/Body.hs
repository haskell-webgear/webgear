{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Body' trait.
module WebGear.OpenApi.Trait.Body where

import Control.Lens ((%~), (&), (.~), (<>~), (?~), (^.))
import Control.Monad.State.Strict (MonadState)
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.OpenApi (
  Definitions,
  MediaTypeObject,
  OpenApi,
  Referenced (..),
  RequestBody,
  Response,
  Schema,
  ToSchema,
  allOperations,
  components,
  content,
  declareSchemaRef,
  description,
  paths,
  requestBody,
  responses,
  schema,
  schemas,
 )
import Data.OpenApi.Declare (runDeclare)
import Data.OpenApi.Internal.Utils (swaggerMappend)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Exts (fromList)
import Network.HTTP.Media.MediaType (MediaType)
import WebGear.Core.Handler (Description (..))
import WebGear.Core.MIMETypes (MIMEType (..))
import WebGear.Core.Request (Request)
import qualified WebGear.Core.Response as WG
import WebGear.Core.Trait (Get (..), Set (..), With)
import WebGear.Core.Trait.Body (Body (..), UnknownContentBody (..))
import WebGear.OpenApi.Handler (
  Documentation (..),
  OpenApiHandler (..),
  addRootPath,
  consumeDescription,
 )

instance (ToSchema val, MIMEType mt) => Get (OpenApiHandler m) (Body mt val) Request where
  {-# INLINE getTrait #-}
  getTrait :: Body mt val -> OpenApiHandler m (Request `With` ts) (Either Text val)
  getTrait (Body mt) =
    OpenApiHandler $ \doc -> do
      desc <- consumeDescription
      let mediaType = mimeType mt
          (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
          body =
            (mempty @RequestBody)
              & content .~ fromList [(mediaType, mempty @MediaTypeObject & schema ?~ ref)]
              & description .~ fmap getDescription desc
      pure $
        doc
          & allOperations . requestBody ?~ Inline body
          & components . schemas %~ (<> defs)

instance (ToSchema val, MIMEType mt) => Set (OpenApiHandler m) (Body mt val) WG.Response where
  {-# INLINE setTrait #-}
  setTrait ::
    Body mt val ->
    (WG.Response `With` ts -> WG.Response -> val -> WG.Response `With` (Body mt val : ts)) ->
    OpenApiHandler m (WG.Response `With` ts, val) (WG.Response `With` (Body mt val : ts))
  setTrait (Body mt) _ =
    let mediaType = mimeType mt
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
        body = mempty @MediaTypeObject & schema ?~ ref
     in OpenApiHandler $ addResponseBody defs (fromList [(mediaType, body)])

instance Set (OpenApiHandler m) UnknownContentBody WG.Response where
  {-# INLINE setTrait #-}
  setTrait ::
    UnknownContentBody ->
    (WG.Response `With` ts -> WG.Response -> WG.ResponseBody -> WG.Response `With` (UnknownContentBody : ts)) ->
    OpenApiHandler m (WG.Response `With` ts, WG.ResponseBody) (WG.Response `With` (UnknownContentBody : ts))
  setTrait UnknownContentBody _ = OpenApiHandler $ addResponseBody mempty mempty

addResponseBody ::
  (MonadState Documentation m) =>
  Definitions Schema ->
  Map.InsOrdHashMap MediaType MediaTypeObject ->
  OpenApi ->
  m OpenApi
addResponseBody defs mediaTypes doc = do
  desc <- consumeDescription

  let addDescription :: Referenced Response -> Referenced Response
      addDescription (Ref r) = Ref r
      addDescription (Inline r) =
        case desc of
          Nothing -> Inline r
          Just (Description d) -> Inline (r & description .~ d)

  let resp = mempty @Response & content <>~ mediaTypes
      doc' = if Map.null (doc ^. paths) then addRootPath doc else doc

  pure $
    doc'
      & allOperations . responses . responses %~ Map.map (addDescription . (`swaggerMappend` Inline resp))
      & components . schemas %~ (<> defs)
