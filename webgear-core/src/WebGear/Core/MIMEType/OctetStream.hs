module WebGear.Core.MIMEType.OctetStream (
  OctetStream,
) where

import Data.Proxy (Proxy)
import qualified Network.HTTP.Media as HTTP
import WebGear.Core.MIMEType (MIMEType (..))

-- | The application/octet-stream MIME type
data OctetStream

instance MIMEType OctetStream where
  mimeType :: Proxy OctetStream -> HTTP.MediaType
  mimeType _ = "application/octet-stream"
