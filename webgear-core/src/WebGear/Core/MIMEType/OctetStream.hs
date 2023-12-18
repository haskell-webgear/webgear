module WebGear.Core.MIMEType.OctetStream (
  OctetStream (..),
) where

import qualified Network.HTTP.Media as HTTP
import WebGear.Core.MIMEType (MIMEType (..))

-- | The application/octet-stream MIME type
data OctetStream = OctetStream

instance MIMEType OctetStream where
  mimeType :: OctetStream -> HTTP.MediaType
  mimeType OctetStream = "application/octet-stream"
  {-# INLINE mimeType #-}
