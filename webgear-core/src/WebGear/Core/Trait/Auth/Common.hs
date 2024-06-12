{- |
 Common types and functions related to authorization.
-}
module WebGear.Core.Trait.Auth.Common (
  AuthorizationHeader,
  Realm (..),
  AuthToken (..),
  respondUnauthorized,
) where

import Data.ByteString (ByteString, drop)
import Data.ByteString.Char8 (break)
import Data.CaseInsensitive (CI, mk, original)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Web.HttpApiData (FromHttpApiData (..))
import WebGear.Core.Handler (Handler, unwitnessA, (>->))
import WebGear.Core.MIMETypes (PlainText (..))
import WebGear.Core.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Sets)
import WebGear.Core.Trait.Body (Body, setBody)
import WebGear.Core.Trait.Header (RequestHeader (..), RequiredResponseHeader, setHeader)
import WebGear.Core.Trait.Status (Status, unauthorized401)
import Prelude hiding (break, drop)

-- | Trait for \"Authorization\" header
type AuthorizationHeader scheme = RequestHeader Optional Lenient "Authorization" (AuthToken scheme)

-- | The protection space for authentication
newtype Realm = Realm ByteString
  deriving newtype (Eq, Ord, Show, Read, IsString)

-- | The components of Authorization request header
data AuthToken (scheme :: Symbol) = AuthToken
  { authScheme :: CI ByteString
  -- ^ Authentication scheme
  , authToken :: ByteString
  -- ^ Authentication token
  }

instance (KnownSymbol scheme) => FromHttpApiData (AuthToken scheme) where
  {-# INLINE parseUrlPiece #-}
  parseUrlPiece :: Text -> Either Text (AuthToken scheme)
  parseUrlPiece = parseHeader . encodeUtf8

  {-# INLINE parseHeader #-}
  parseHeader :: ByteString -> Either Text (AuthToken scheme)
  parseHeader hdr =
    case break (== ' ') hdr of
      (scm, tok) ->
        let actualScheme = mk scm
            expectedScheme = fromString $ symbolVal $ Proxy @scheme
         in if actualScheme == expectedScheme
              then Right (AuthToken actualScheme (drop 1 tok))
              else Left "scheme mismatch"

{- | Create a \"401 Unauthorized\" response.

 The response will have a plain text body and an appropriate
 \"WWW-Authenticate\" header.
-}
respondUnauthorized ::
  ( Handler h m
  , Sets
      h
      [ Status
      , RequiredResponseHeader "Content-Type" Text
      , RequiredResponseHeader "WWW-Authenticate" Text
      , Body PlainText Text
      ]
  ) =>
  -- | The authentication scheme
  CI ByteString ->
  -- | The authentication realm
  Realm ->
  h a Response
respondUnauthorized scheme (Realm realm) = proc _ -> do
  let headerVal = decodeUtf8 $ original scheme <> " realm=\"" <> realm <> "\""
  (unauthorized401 -< ())
    >-> (\resp -> setBody PlainText -< (resp, "Unauthorized" :: Text))
    >-> (\resp -> setHeader @"WWW-Authenticate" -< (resp, headerVal))
    >-> (\resp -> unwitnessA -< resp)
{-# INLINE respondUnauthorized #-}
