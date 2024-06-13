{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Header' trait.
module WebGear.OpenApi.Trait.Header () where

import Control.Lens ((%~), (&), (.~), (<>~), (?~))
import Control.Monad.State.Strict (MonadState)
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.OpenApi (
  Header,
  HeaderName,
  OpenApi,
  Param,
  ParamLocation (..),
  Referenced (..),
  Response,
  ToSchema,
  allOperations,
  description,
  headers,
  in_,
  name,
  parameters,
  required,
  responses,
  schema,
  toSchema,
 )
import Data.OpenApi.Internal.Utils (swaggerMappend)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Handler (Description (..))
import WebGear.Core.Modifiers (Existence (..))
import WebGear.Core.Trait (Get (..), Set (..))
import qualified WebGear.Core.Trait.Header as WG
import WebGear.OpenApi.Handler (Documentation, OpenApiHandler (..), consumeDescription)

instance (KnownSymbol name, ToSchema val) => Get (OpenApiHandler m) (WG.RequestHeader Required ps name val) where
  {-# INLINE getTrait #-}
  getTrait WG.RequestHeader = OpenApiHandler $ addRequestHeader (Proxy @name) (Proxy @val) True

instance (KnownSymbol name, ToSchema val) => Get (OpenApiHandler m) (WG.RequestHeader Optional ps name val) where
  {-# INLINE getTrait #-}
  getTrait WG.RequestHeader = OpenApiHandler $ addRequestHeader (Proxy @name) (Proxy @val) False

instance (KnownSymbol name, ToSchema val) => Set (OpenApiHandler m) (WG.ResponseHeader Required name val) where
  {-# INLINE setTrait #-}
  setTrait WG.ResponseHeader _ = OpenApiHandler $ addResponseHeader (Proxy @name) (Proxy @val) True

instance (KnownSymbol name, ToSchema val) => Set (OpenApiHandler m) (WG.ResponseHeader Optional name val) where
  {-# INLINE setTrait #-}
  setTrait WG.ResponseHeader _ = OpenApiHandler $ addResponseHeader (Proxy @name) (Proxy @val) False

addRequestHeader ::
  forall name val m.
  (KnownSymbol name, ToSchema val, MonadState Documentation m) =>
  Proxy name ->
  Proxy val ->
  Bool ->
  OpenApi ->
  m OpenApi
addRequestHeader _ _ isRequired doc = do
  desc <- consumeDescription
  let param =
        (mempty :: Param)
          & name .~ fromString @Text (symbolVal $ Proxy @name)
          & in_ .~ ParamHeader
          & required ?~ isRequired
          & schema ?~ Inline (toSchema $ Proxy @val)
          & description .~ fmap getDescription desc
  pure $ doc & allOperations . parameters <>~ [Inline param]

addResponseHeader ::
  forall name val m.
  (KnownSymbol name, ToSchema val, MonadState Documentation m) =>
  Proxy name ->
  Proxy val ->
  Bool ->
  OpenApi ->
  m OpenApi
addResponseHeader _ _ isRequired doc = do
  desc <- consumeDescription
  let headerName = fromString @HeaderName $ symbolVal $ Proxy @name
      header =
        mempty @Header
          & required ?~ isRequired
          & schema ?~ Inline (toSchema $ Proxy @val)
          & description .~ fmap getDescription desc
      resp = mempty @Response & headers <>~ [(headerName, Inline header)]
  pure $
    if headerName == "Content-Type"
      then doc
      else doc & allOperations . responses . responses %~ Map.map (`swaggerMappend` Inline resp)
