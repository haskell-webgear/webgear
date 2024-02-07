{- |
 Responses from handlers.
-}
module WebGear.Core.Response (
  -- * Basic Types
  Response (..),
  ResponseBody (..),
) where

import qualified Data.Binary.Builder as B
import Data.ByteString (ByteString)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

-- | An HTTP response sent from the server to the client.
data Response
  = Response HTTP.Status HTTP.ResponseHeaders ResponseBody
  | ResponseRaw (IO ByteString -> (ByteString -> IO ()) -> IO ()) Wai.Response
  | ResponseCont ((Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived)

-- | HTTP response body
data ResponseBody
  = ResponseBodyFile FilePath (Maybe Wai.FilePart)
  | ResponseBodyBuilder B.Builder
  | ResponseBodyStream Wai.StreamingBody
