{- |
 Common types and functions related to authorization.
-}
module WebGear.Core.Middleware.Auth.Common (
  AuthorizationHeader,
  getAuthorizationHeaderTrait,
  Realm (..),
  AuthToken (..),
  respondUnauthorized,
) where

import Control.Arrow (returnA, (<<<))
import Data.ByteString (ByteString, drop)
import Data.ByteString.Char8 (break)
import Data.CaseInsensitive (CI, mk, original)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Void (absurd)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Types as HTTP
import Web.HttpApiData (FromHttpApiData (..))
import WebGear.Core.Handler (Handler, unlinkA)
import WebGear.Core.Middleware.Body (Body, respondA)
import WebGear.Core.Middleware.Header (Header (..), RequiredHeader, setHeader)
import WebGear.Core.Middleware.Status (Status)
import WebGear.Core.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get (..), Linked, Sets)
import Prelude hiding (break, drop)

-- | Trait for \"Authorization\" header
type AuthorizationHeader scheme = Header Optional Lenient "Authorization" (AuthToken scheme)

{- | Extract the \"Authorization\" header from a request by specifying
   an authentication scheme.

  The header is split into the scheme and token parts and returned.
-}
getAuthorizationHeaderTrait ::
  forall scheme h ts.
  Get h (AuthorizationHeader scheme) Request =>
  h (Linked ts Request) (Maybe (Either Text (AuthToken scheme)))
getAuthorizationHeaderTrait = proc request -> do
  result <- getTrait (Header :: Header Optional Lenient "Authorization" (AuthToken scheme)) -< request
  returnA -< either absurd id result

-- | The protection space for authentication
newtype Realm = Realm ByteString
  deriving newtype (Eq, Ord, Show, Read, IsString)

-- | The components of Authorization request header
data AuthToken (scheme :: Symbol) = AuthToken
  { -- | Authentication scheme
    authScheme :: CI ByteString
  , -- | Authentication token
    authToken :: ByteString
  }

instance KnownSymbol scheme => FromHttpApiData (AuthToken scheme) where
  parseUrlPiece = parseHeader . encodeUtf8

  parseHeader hdr =
    case break (== ' ') hdr of
      (scm, tok) ->
        let actualScheme = mk scm
            expectedScheme = fromString $ symbolVal $ Proxy @scheme
         in if actualScheme == expectedScheme
              then Right (AuthToken actualScheme (drop 1 tok))
              else Left "scheme mismatch"

{- | Create a \"401 Unquthorized\" response.

 The response will have a plain text body and an appropriate
 \"WWW-Authenticate\" header.
-}
respondUnauthorized ::
  ( Handler h m
  , Sets
      h
      [ Status
      , RequiredHeader "Content-Type" Text
      , RequiredHeader "WWW-Authenticate" Text
      , Body Text
      ]
      Response
  ) =>
  -- | The authentication scheme
  CI ByteString ->
  -- | The authentication realm
  Realm ->
  h a Response
respondUnauthorized scheme (Realm realm) = proc _ -> do
  let headerVal = decodeUtf8 $ original scheme <> " realm=\"" <> realm <> "\""
  r <- respondA HTTP.unauthorized401 "text/plain" -< "Unauthorized" :: Text
  unlinkA <<< setHeader @"WWW-Authenticate" -< (r, headerVal)
