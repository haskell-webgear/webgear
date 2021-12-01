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

import Control.Arrow (returnA)
import Data.ByteString (ByteString, drop)
import Data.ByteString.Char8 (break)
import Data.CaseInsensitive (CI, mk, original)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Void (absurd)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Web.HttpApiData (FromHttpApiData (..))
import WebGear.Core.Handler (Handler, respondA, (>#>))
import WebGear.Core.Middleware.Body (Body, setBody)
import WebGear.Core.Middleware.Header (Header (..), RequiredHeader, setHeader)
import WebGear.Core.Middleware.Status (Status, unauthorized401)
import WebGear.Core.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get (..), Linked, Sets)
import Prelude hiding (break, drop)

-- | Header trait for authorization
type AuthorizationHeader scheme = Header Optional Lenient "Authorization" (AuthToken scheme)

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

data AuthToken (scheme :: Symbol) = AuthToken
  { authScheme :: CI ByteString
  , authToken :: ByteString
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

respondUnauthorized ::
  ( Handler h m
  , Sets h [Status, RequiredHeader "Content-Type" Text, RequiredHeader "WWW-Authenticate" ByteString, Body (Just "text/plain") Text] Response
  ) =>
  CI ByteString ->
  Realm ->
  h a Response
respondUnauthorized scheme (Realm realm) = proc _ -> do
  let body = "Unauthorized" :: Text
      headerVal = original scheme <> " realm=\"" <> realm <> "\""
  r <-
    (unauthorized401 -< ()) >#>
      (\r -> setBody @"text/plain" -< (r, body)) >#>
      (\r -> setHeader @"WWW-Authenticate" -< (r, headerVal))
  respondA -< r
