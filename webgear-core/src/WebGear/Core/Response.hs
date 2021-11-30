-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- WebGear handler responses
--
module WebGear.Core.Response
  ( -- * Basic Types
    Response (..)
  , toWaiResponse
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai


--- | An HTTP response sent from the server to the client.
---
--- The response contains a status, optional headers and an optional
--- body payload.
data Response = Response
    { responseStatus  :: HTTP.Status                            -- ^ Response status code
    , responseHeaders :: HM.HashMap HTTP.HeaderName ByteString  -- ^ Response headers
    , responseBody    :: Maybe LBS.ByteString                   -- ^ Optional response body
    }
    deriving stock (Eq, Ord, Show)

toWaiResponse :: Response -> Wai.Response
toWaiResponse Response{..} =
  Wai.responseLBS responseStatus (HM.toList responseHeaders) (fromMaybe "" responseBody)
