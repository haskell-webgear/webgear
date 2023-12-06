{-# OPTIONS_GHC -Wno-orphans #-}

-- | Swagger implementation of 'Header' trait.
module WebGear.Swagger.Trait.Header () where

import Control.Lens ((&), (.~), (?~))
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Swagger hiding (Response)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Modifiers (Existence (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get (..), Set (..), TraitAbsence)
import qualified WebGear.Core.Trait.Header as WG
import WebGear.Swagger.Handler (DocNode (..), SwaggerHandler (..), nullNode, singletonNode)

mkParam ::
  forall name val.
  (KnownSymbol name, ToParamSchema val) =>
  Proxy name ->
  Proxy val ->
  Bool ->
  Param
mkParam proxyName proxyVal isRequired =
  (mempty :: Param)
    & name .~ fromString @Text (symbolVal proxyName)
    & required ?~ isRequired
    & schema
      .~ ParamOther
        ( ParamOtherSchema
            { _paramOtherSchemaIn = ParamHeader
            , _paramOtherSchemaAllowEmptyValue = Just (not isRequired)
            , _paramOtherSchemaParamSchema = toParamSchema proxyVal
            }
        )

instance
  ( KnownSymbol name
  , ToParamSchema val
  , TraitAbsence (WG.RequestHeader Required ps name val) Request
  ) =>
  Get (SwaggerHandler m) (WG.RequestHeader Required ps name val) Request
  where
  {-# INLINE getTrait #-}
  getTrait WG.RequestHeader =
    SwaggerHandler $ singletonNode (DocRequestHeader $ mkParam (Proxy @name) (Proxy @val) True)

instance
  ( KnownSymbol name
  , ToParamSchema val
  , TraitAbsence (WG.RequestHeader Optional ps name val) Request
  ) =>
  Get (SwaggerHandler m) (WG.RequestHeader Optional ps name val) Request
  where
  {-# INLINE getTrait #-}
  getTrait WG.RequestHeader =
    SwaggerHandler $ singletonNode (DocRequestHeader $ mkParam (Proxy @name) (Proxy @val) False)

instance (KnownSymbol name) => Set (SwaggerHandler m) (WG.ResponseHeader Required name val) Response where
  {-# INLINE setTrait #-}
  setTrait WG.ResponseHeader _ =
    let headerName = fromString $ symbolVal $ Proxy @name
        header = mempty @Header
     in if headerName == "Content-Type"
          then SwaggerHandler nullNode
          else SwaggerHandler $ singletonNode (DocResponseHeader headerName header)

instance (KnownSymbol name) => Set (SwaggerHandler m) (WG.ResponseHeader Optional name val) Response where
  {-# INLINE setTrait #-}
  setTrait WG.ResponseHeader _ =
    let headerName = fromString $ symbolVal $ Proxy @name
        header = mempty @Header
     in SwaggerHandler $ singletonNode (DocResponseHeader headerName header)
