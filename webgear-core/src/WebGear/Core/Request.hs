{- |
 Requests processed by handlers.
-}
module WebGear.Core.Request (
  -- * WebGear Request
  Request (..),
  remoteHost,
  httpVersion,
  isSecure,
  requestMethod,
  pathInfo,
  queryString,
  requestHeader,
  requestHeaders,
  requestBodyLength,
  getRequestBodyChunk,
) where

import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import qualified Network.HTTP.Types as HTTP
import Network.Socket (SockAddr)
import qualified Network.Wai as Wai

-- | A request processed by a handler
newtype Request = Request
  { toWaiRequest :: Wai.Request
  -- ^ underlying WAI request
  }

-- | Get the value of a request header
requestHeader :: HTTP.HeaderName -> Request -> Maybe ByteString
requestHeader h r = snd <$> find ((== h) . fst) (requestHeaders r)

-- | See 'Wai.getRequestBodyChunk'
getRequestBodyChunk :: Request -> IO ByteString
getRequestBodyChunk = Wai.getRequestBodyChunk . toWaiRequest

-- | See 'Wai.httpVersion'
httpVersion :: Request -> HTTP.HttpVersion
httpVersion = Wai.httpVersion . toWaiRequest

-- | See 'Wai.isSecure'
isSecure :: Request -> Bool
isSecure = Wai.isSecure . toWaiRequest

-- | See 'Wai.pathInfo'
pathInfo :: Request -> [Text]
pathInfo = Wai.pathInfo . toWaiRequest

-- | See 'Wai.queryString'
queryString :: Request -> HTTP.Query
queryString = Wai.queryString . toWaiRequest

-- | See 'Wai.remoteHost'
remoteHost :: Request -> SockAddr
remoteHost = Wai.remoteHost . toWaiRequest

-- | See 'Wai.requestBodyLength'
requestBodyLength :: Request -> Wai.RequestBodyLength
requestBodyLength = Wai.requestBodyLength . toWaiRequest

-- | See 'Wai.requestHeaders'
requestHeaders :: Request -> HTTP.RequestHeaders
requestHeaders = Wai.requestHeaders . toWaiRequest

-- | See 'Wai.requestMethod'
requestMethod :: Request -> HTTP.Method
requestMethod = Wai.requestMethod . toWaiRequest
