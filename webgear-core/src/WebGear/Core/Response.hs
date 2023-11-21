{- |
 Responses from handlers.
-}
module WebGear.Core.Response (
  -- * Basic Types
  Response (..),
  ResponseBody (..),
  toWaiResponse,
) where

import qualified Data.Binary.Builder as B
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

{- | An HTTP response sent from the server to the client.

The response contains a status, optional headers and an optional
body payload.
-}
data Response = Response
  { responseStatus :: HTTP.Status
  -- ^ Response status code
  , responseHeaders :: HTTP.ResponseHeaders
  -- ^ Response headers
  , responseBody :: ResponseBody
  -- ^ The response body
  }

data ResponseBody
  = ResponseBodyFile FilePath (Maybe Wai.FilePart)
  | ResponseBodyBuilder B.Builder
  | ResponseBodyStream Wai.StreamingBody

-- | Generate a WAI response
toWaiResponse :: Response -> Wai.Response
toWaiResponse Response{..} =
  case responseBody of
    ResponseBodyFile fpath fpart ->
      Wai.responseFile responseStatus responseHeaders fpath fpart
    ResponseBodyBuilder builder ->
      Wai.responseBuilder responseStatus responseHeaders builder
    ResponseBodyStream stream ->
      Wai.responseStream responseStatus responseHeaders stream
