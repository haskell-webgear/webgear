module WebGear.Core.MIMEType.JSON (
  JSON',
  JSON,
) where

import Data.Proxy (Proxy (..))
import Data.String (fromString)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Media as HTTP
import WebGear.Core.MIMEType (MIMEType (..))

-- | A JSON MIME type with customizable media type
data JSON' (mediaType :: Symbol)

instance (KnownSymbol mt) => MIMEType (JSON' mt) where
  mimeType :: Proxy (JSON' mt) -> HTTP.MediaType
  mimeType _ = fromString $ symbolVal (Proxy @mt)

-- | The application/json MIME type
type JSON = JSON' "application/json"
