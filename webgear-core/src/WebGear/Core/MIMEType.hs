-- | MIME types for HTTP bodies
module WebGear.Core.MIMEType (
  MIMEType (..),
) where

import qualified Network.HTTP.Media as HTTP

-- | MIME types used in the Accept and Content-Type headers
class MIMEType mt where
  mimeType :: mt -> HTTP.MediaType
