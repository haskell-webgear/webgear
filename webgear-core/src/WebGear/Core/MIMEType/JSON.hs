module WebGear.Core.MIMEType.JSON (
  JSON (..),
) where

import Data.String (fromString)
import Data.Text (Text, unpack)
import qualified Network.HTTP.Media as HTTP
import WebGear.Core.MIMEType (MIMEType (..))

-- | A JSON MIME type with customizable media type
data JSON
  = -- | JSON with a specific media type
    JSONMedia Text
  | -- | application/json media type
    JSON

instance MIMEType JSON where
  mimeType :: JSON -> HTTP.MediaType
  mimeType =
    \case
      JSONMedia mt -> fromString (unpack mt)
      JSON -> "application/json"
  {-# INLINE mimeType #-}
