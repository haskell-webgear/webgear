-- | Parsing and generating multipart/form-data content
module WebGear.Core.MIMEType.MultiPartFormData (
  -- * Parsing form data
  FormData (..),
  FormDataResult (..),
) where

import Network.HTTP.Media (MediaType)
import Network.Wai.Parse (
  BackEnd,
  File,
  Param,
  ParseRequestBodyOptions,
 )
import WebGear.Core.MIMEType (MIMEType (..))

-- | Represents multipart/form-data MIME type in a request
data FormData a = FormData
  { parseOptions :: ParseRequestBodyOptions
  , backendOptions :: BackEnd a
  }

{- | Result of parsing a multipart/form-data body from a request.
The body can contain both parameters and files.
-}
data FormDataResult a = FormDataResult
  { formDataParams :: [Param]
  , formDataFiles :: [File a]
  }

instance MIMEType (FormData a) where
  mimeType :: FormData a -> MediaType
  mimeType _ = "multipart/form-data"
  {-# INLINE mimeType #-}
