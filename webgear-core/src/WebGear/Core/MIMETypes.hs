-- | MIME types for HTTP bodies
module WebGear.Core.MIMETypes (
  MIMEType (..),
  FormURLEncoded (..),
  HTML (..),
  JSON (..),
  FormData (..),
  FormDataResult (..),
  OctetStream (..),
  PlainText (..),
) where

import Data.String (IsString (fromString))
import Data.Text (Text, unpack)
import qualified Network.HTTP.Media as HTTP
import qualified Network.Wai.Parse as Wai.Parse

-- | MIME types used in the Accept and Content-Type headers
class MIMEType mt where
  mimeType :: mt -> HTTP.MediaType

--------------------------------------------------------------------------------

-- | The application/x-www-form-urlencoded MIME type
data FormURLEncoded = FormURLEncoded

instance MIMEType FormURLEncoded where
  mimeType :: FormURLEncoded -> HTTP.MediaType
  mimeType FormURLEncoded = "application/x-www-form-urlencoded"
  {-# INLINE mimeType #-}

--------------------------------------------------------------------------------

-- | The text/html MIME type
data HTML = HTML

instance MIMEType HTML where
  mimeType :: HTML -> HTTP.MediaType
  mimeType HTML = "text/html"
  {-# INLINE mimeType #-}

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- | The multipart/form-data MIME type
data FormData m a = FormData
  { parseOptions :: Wai.Parse.ParseRequestBodyOptions
  , backendOptions :: m (Wai.Parse.BackEnd a)
  }

{- | Result of parsing a multipart/form-data body from a request.
The body can contain both parameters and files.
-}
data FormDataResult a = FormDataResult
  { formDataParams :: [Wai.Parse.Param]
  , formDataFiles :: [Wai.Parse.File a]
  }

instance MIMEType (FormData m a) where
  mimeType :: FormData m a -> HTTP.MediaType
  mimeType _ = "multipart/form-data"
  {-# INLINE mimeType #-}

--------------------------------------------------------------------------------

-- | The application/octet-stream MIME type
data OctetStream = OctetStream

instance MIMEType OctetStream where
  mimeType :: OctetStream -> HTTP.MediaType
  mimeType OctetStream = "application/octet-stream"
  {-# INLINE mimeType #-}

--------------------------------------------------------------------------------

-- | The text/plain MIME type
data PlainText = PlainText

instance MIMEType PlainText where
  mimeType :: PlainText -> HTTP.MediaType
  mimeType PlainText = "text/plain"
  {-# INLINE mimeType #-}
