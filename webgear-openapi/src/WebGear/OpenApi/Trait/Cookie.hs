{-# OPTIONS_GHC -Wno-orphans #-}

-- | OpenApi implementation of 'WG.Cookie' and 'WG.SetCookie' traits.
module WebGear.OpenApi.Trait.Cookie () where

import Data.OpenApi hiding (Response)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal)
import WebGear.Core.Trait (Get (..), Set (..))
import qualified WebGear.Core.Trait.Cookie as WG
import WebGear.OpenApi.Handler (OpenApiHandler (..))
import WebGear.OpenApi.Trait.Auth (addSecurityScheme)

instance (KnownSymbol name) => Get (OpenApiHandler m) (WG.Cookie e name val) where
  {-# INLINE getTrait #-}
  getTrait WG.Cookie =
    OpenApiHandler $ addSecurityScheme cookieName securityScheme
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

instance Set (OpenApiHandler m) (WG.SetCookie e name) where
  {-# INLINE setTrait #-}
  setTrait WG.SetCookie _ = OpenApiHandler pure
