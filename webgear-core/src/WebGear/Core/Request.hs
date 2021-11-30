-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Requests processed by handlers.
--
module WebGear.Core.Request
  ( -- * WebGear Request
    Request (..)
  , remoteHost
  , httpVersion
  , isSecure
  , requestMethod
  , pathInfo
  , queryString
  , requestHeader
  , requestHeaders
  , requestBodyLength
  , getRequestBodyChunk
  ) where

import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import qualified Network.HTTP.Types as HTTP
import Network.Socket (SockAddr)
import qualified Network.Wai as Wai


newtype Request = Request
  { waiRequest   :: Wai.Request
    -- ^ underlying WAI request
  }

-- | Get the value of a request header
requestHeader :: HTTP.HeaderName -> Request -> Maybe ByteString
requestHeader h r = snd <$> find ((== h) . fst) (requestHeaders r)

-- | See 'Wai.getRequestBodyChunk'
getRequestBodyChunk :: Request -> IO ByteString
getRequestBodyChunk = Wai.getRequestBodyChunk . waiRequest

-- | See 'Wai.httpVersion'
httpVersion :: Request -> HTTP.HttpVersion
httpVersion = Wai.httpVersion . waiRequest

-- | See 'Wai.isSecure'
isSecure :: Request -> Bool
isSecure = Wai.isSecure . waiRequest

-- | See 'Wai.pathInfo'
pathInfo :: Request -> [Text]
pathInfo = Wai.pathInfo . waiRequest

-- | See 'Wai.queryString'
queryString :: Request -> HTTP.Query
queryString = Wai.queryString . waiRequest

-- | See 'Wai.remoteHost'
remoteHost :: Request -> SockAddr
remoteHost = Wai.remoteHost . waiRequest

-- | See 'Wai.requestBodyLength'
requestBodyLength :: Request -> Wai.RequestBodyLength
requestBodyLength = Wai.requestBodyLength . waiRequest

-- | See 'Wai.requestHeaders'
requestHeaders :: Request -> HTTP.RequestHeaders
requestHeaders = Wai.requestHeaders . waiRequest

-- | See 'Wai.requestMethod'
requestMethod :: Request -> HTTP.Method
requestMethod = Wai.requestMethod . waiRequest
