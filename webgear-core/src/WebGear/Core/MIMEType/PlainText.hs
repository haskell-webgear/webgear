module WebGear.Core.MIMEType.PlainText (
  PlainText (..),
) where

import Data.String (fromString)
import Data.Text (Text, unpack)
import qualified Network.HTTP.Media as HTTP
import WebGear.Core.MIMEType (MIMEType (..))

data PlainText
  = -- | text/plain with a specific charset
    PlainTextCharSet Text
  | -- | text/plain without a charset
    PlainText

instance MIMEType PlainText where
  mimeType :: PlainText -> HTTP.MediaType
  mimeType =
    \case
      PlainTextCharSet cs -> fromString $ "text/plain" <> unpack cs
      PlainText -> "text/plain"
  {-# INLINE mimeType #-}
