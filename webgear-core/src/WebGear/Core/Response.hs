{- |
 Responses from handlers.
-}
module WebGear.Core.Response (
  -- * Basic Types
  Response (..),
  toWaiResponse,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

-- | An HTTP response sent from the server to the client.
--
-- The response contains a status, optional headers and an optional
-- body payload.
data Response = Response
  { -- | Response status code
    responseStatus :: HTTP.Status
  , -- | Response headers
    responseHeaders :: HM.HashMap HTTP.HeaderName ByteString
  , -- | Optional response body
    responseBody :: Maybe LBS.ByteString
  }
  deriving stock (Eq, Ord, Show)

-- | Generate a WAI response
toWaiResponse :: Response -> Wai.Response
toWaiResponse Response{..} =
  Wai.responseLBS responseStatus (HM.toList responseHeaders) (fromMaybe "" responseBody)
