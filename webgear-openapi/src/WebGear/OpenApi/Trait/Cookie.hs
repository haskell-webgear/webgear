{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'Cookie' and 'SetCookie' traits.
module WebGear.OpenApi.Trait.Cookie () where

import Data.OpenApi hiding (Response)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get (..), Set (..), Trait, TraitAbsence)
import qualified WebGear.Core.Trait.Cookie as WG
import WebGear.OpenApi.Handler (DocNode (..), OpenApiHandler (..), nullNode, singletonNode)

instance (KnownSymbol name, TraitAbsence (WG.Cookie e name val) Request) => Get (OpenApiHandler m) (WG.Cookie e name val) Request where
  {-# INLINE getTrait #-}
  getTrait WG.Cookie =
    OpenApiHandler $ singletonNode $ DocSecurityScheme cookieName securityScheme
    where
      cookieName = fromString @Text $ symbolVal $ Proxy @name

      securityScheme :: SecurityScheme
      securityScheme =
        SecurityScheme
          { _securitySchemeType =
              SecuritySchemeApiKey
                ApiKeyParams
                  { _apiKeyName = cookieName
                  , _apiKeyIn = ApiKeyCookie
                  }
          , _securitySchemeDescription = Nothing
          }

-- Response cookie information is not captured by OpenAPI

instance (Trait (WG.SetCookie e name) Response) => Set (OpenApiHandler m) (WG.SetCookie e name) Response where
  {-# INLINE setTrait #-}
  setTrait WG.SetCookie _ = OpenApiHandler nullNode
