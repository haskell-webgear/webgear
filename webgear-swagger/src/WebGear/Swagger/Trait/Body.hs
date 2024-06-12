{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'Body' trait.
module WebGear.Swagger.Trait.Body where

import Control.Lens ((%~), (&), (.~), (?~), (^.))
import Control.Monad.State.Strict (MonadState)
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.Proxy (Proxy (..))
import Data.Swagger (
  Definitions,
  MimeList (..),
  Param,
  ParamAnySchema (..),
  Referenced (..),
  Response,
  Schema,
  Swagger,
  ToSchema,
  allOperations,
  consumes,
  declareSchemaRef,
  definitions,
  description,
  name,
  parameters,
  paths,
  produces,
  required,
  responses,
  schema,
 )
import Data.Swagger.Declare (runDeclare)
import Data.Swagger.Internal.Utils (swaggerMappend)
import Data.Text (Text)
import WebGear.Core.Handler (Description (..))
import WebGear.Core.MIMETypes (MIMEType (..))
import WebGear.Core.Request (Request)
import qualified WebGear.Core.Response as WG
import WebGear.Core.Trait (Get (..), Set (..), With)
import WebGear.Core.Trait.Body (Body (..), UnknownContentBody (..))
import WebGear.Swagger.Handler (
  Documentation (..),
  SwaggerHandler (..),
  addRootPath,
  consumeDescription,
 )

instance (ToSchema val, MIMEType mt) => Get (SwaggerHandler m) (Body mt val) where
  {-# INLINE getTrait #-}
  getTrait :: Body mt val -> SwaggerHandler m (Request `With` ts) (Either Text val)
  getTrait (Body mt) =
    SwaggerHandler $ \doc -> do
      desc <- consumeDescription
      let mimeList = MimeList [mimeType mt]
          (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
          body =
            (mempty @Param)
              & schema .~ ParamBody ref
              & required ?~ True
              & name .~ "body"
              & description .~ fmap getDescription desc
      pure $
        doc
          & allOperations
            %~ ( \op ->
                  op
                    & parameters %~ (Inline body :)
                    & consumes %~ Just . maybe mimeList (<> mimeList)
               )
          & definitions %~ (<> defs)

instance (ToSchema val, MIMEType mt) => Set (SwaggerHandler m) (Body mt val) where
  {-# INLINE setTrait #-}
  setTrait ::
    Body mt val ->
    (WG.Response `With` ts -> WG.Response -> val -> WG.Response `With` (Body mt val : ts)) ->
    SwaggerHandler m (WG.Response `With` ts, val) (WG.Response `With` (Body mt val : ts))
  setTrait (Body mt) _ =
    let mimeList = MimeList [mimeType mt]
        (defs, ref) = runDeclare (declareSchemaRef $ Proxy @val) mempty
     in SwaggerHandler $ addResponseBody defs mimeList (Just ref)

instance Set (SwaggerHandler m) UnknownContentBody where
  {-# INLINE setTrait #-}
  setTrait ::
    UnknownContentBody ->
    (WG.Response `With` ts -> WG.Response -> WG.ResponseBody -> WG.Response `With` (UnknownContentBody : ts)) ->
    SwaggerHandler m (WG.Response `With` ts, WG.ResponseBody) (WG.Response `With` (UnknownContentBody : ts))
  setTrait UnknownContentBody _ = SwaggerHandler $ addResponseBody mempty mempty Nothing

addResponseBody ::
  (MonadState Documentation m) =>
  Definitions Schema ->
  MimeList ->
  Maybe (Referenced Schema) ->
  Swagger ->
  m Swagger
addResponseBody defs mimeList respSchema doc = do
  desc <- consumeDescription

  let addDescription :: Referenced Response -> Referenced Response
      addDescription (Ref r) = Ref r
      addDescription (Inline r) =
        case desc of
          Nothing -> Inline r
          Just (Description d) -> Inline (r & description .~ d)

  let resp = mempty @Response & schema .~ respSchema
      doc' = if Map.null (doc ^. paths) then addRootPath doc else doc

  pure $
    doc'
      & allOperations
        %~ ( \op ->
              op
                & responses . responses %~ Map.map (addDescription . (`swaggerMappend` Inline resp))
                & produces %~ Just . maybe mimeList (`swaggerMappend` mimeList)
           )
      & definitions %~ (<> defs)
