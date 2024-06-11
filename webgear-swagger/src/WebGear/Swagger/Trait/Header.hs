{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'Header' trait.
module WebGear.Swagger.Trait.Header () where

import Control.Lens ((%~), (&), (.~), (<>~), (?~))
import Control.Monad.State.Strict (MonadState)
import qualified Data.HashMap.Strict.InsOrd as Map
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Swagger (
  Header,
  HeaderName,
  Param,
  ParamAnySchema (..),
  ParamLocation (..),
  ParamOtherSchema (..),
  Referenced (..),
  Response,
  Swagger,
  ToParamSchema (..),
  allOperations,
  description,
  headers,
  name,
  parameters,
  required,
  responses,
  schema,
 )
import Data.Swagger.Internal.Utils (swaggerMappend)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Handler (Description (..))
import WebGear.Core.Modifiers (Existence (..))
import WebGear.Core.Request (Request)
import qualified WebGear.Core.Response as WG
import WebGear.Core.Trait (Get (..), Set (..))
import qualified WebGear.Core.Trait.Header as WG
import WebGear.Swagger.Handler (Documentation, SwaggerHandler (..), consumeDescription)

instance
  ( KnownSymbol name
  , ToParamSchema val
  ) =>
  Get (SwaggerHandler m) (WG.RequestHeader Required ps name val) Request
  where
  {-# INLINE getTrait #-}
  getTrait WG.RequestHeader = SwaggerHandler $ addRequestHeader (Proxy @name) (Proxy @val) True

instance
  ( KnownSymbol name
  , ToParamSchema val
  ) =>
  Get (SwaggerHandler m) (WG.RequestHeader Optional ps name val) Request
  where
  {-# INLINE getTrait #-}
  getTrait WG.RequestHeader = SwaggerHandler $ addRequestHeader (Proxy @name) (Proxy @val) False

instance (KnownSymbol name) => Set (SwaggerHandler m) (WG.ResponseHeader Required name val) WG.Response where
  {-# INLINE setTrait #-}
  setTrait WG.ResponseHeader _ = SwaggerHandler $ addResponseHeader (Proxy @name) (Proxy @val)

instance (KnownSymbol name) => Set (SwaggerHandler m) (WG.ResponseHeader Optional name val) WG.Response where
  {-# INLINE setTrait #-}
  setTrait WG.ResponseHeader _ = SwaggerHandler $ addResponseHeader (Proxy @name) (Proxy @val)

addRequestHeader ::
  forall name val m.
  (KnownSymbol name, ToParamSchema val, MonadState Documentation m) =>
  Proxy name ->
  Proxy val ->
  Bool ->
  Swagger ->
  m Swagger
addRequestHeader _ _ isRequired doc = do
  desc <- consumeDescription
  let param =
        (mempty :: Param)
          & name .~ fromString @Text (symbolVal $ Proxy @name)
          & required ?~ isRequired
          & schema
            .~ ParamOther
              ( ParamOtherSchema
                  { _paramOtherSchemaIn = ParamHeader
                  , _paramOtherSchemaAllowEmptyValue = Just (not isRequired)
                  , _paramOtherSchemaParamSchema = toParamSchema (Proxy @val)
                  }
              )
          & description .~ fmap getDescription desc
  pure $ doc & allOperations . parameters <>~ [Inline param]

addResponseHeader ::
  forall name val m.
  (KnownSymbol name, MonadState Documentation m) =>
  Proxy name ->
  Proxy val ->
  Swagger ->
  m Swagger
addResponseHeader _ _ doc = do
  desc <- consumeDescription
  let headerName = fromString @HeaderName $ symbolVal $ Proxy @name
      header = mempty @Header & description .~ fmap getDescription desc
      resp = mempty @Response & headers <>~ [(headerName, header)]
  pure $
    if headerName == "Content-Type"
      then doc
      else doc & allOperations . responses . responses %~ Map.map (`swaggerMappend` Inline resp)
