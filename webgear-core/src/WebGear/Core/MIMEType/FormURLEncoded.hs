module WebGear.Core.MIMEType.FormURLEncoded (
  FormURLEncoded (..),
) where

import qualified Network.HTTP.Media as HTTP
import WebGear.Core.MIMEType (MIMEType (..))

-- | Represents application/x-www-form-urlencoded MIME type
data FormURLEncoded = FormURLEncoded

instance MIMEType FormURLEncoded where
  mimeType :: FormURLEncoded -> HTTP.MediaType
  mimeType FormURLEncoded = "application/x-www-form-urlencoded"
  {-# INLINE mimeType #-}
