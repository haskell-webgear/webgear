{-# LANGUAGE DuplicateRecordFields #-}

{- |
 Basic authentication support.
-}
module WebGear.Core.Middleware.Auth.Basic (
  BasicAuth' (..),
  BasicAuth,
  BasicAuthConfig (..),
  Realm (..),
  Username (..),
  Password (..),
  Credentials (..),
  BasicAuthError (..),
  basicAuth,
  optionalBasicAuth,
) where

import Control.Arrow (ArrowChoice, arr)
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Void (Void, absurd)
import GHC.TypeLits (Symbol)
import WebGear.Core.Handler (Middleware)
import WebGear.Core.Middleware.Auth.Common (Realm (..))
import WebGear.Core.Modifiers (Existence (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get, Linked, Trait (..), TraitAbsence (..), probe)

-- | Trait for HTTP basic authentication: https://tools.ietf.org/html/rfc7617
newtype BasicAuth' (x :: Existence) (scheme :: Symbol) m e a = BasicAuth'
  { toBasicAttribute :: Credentials -> m (Either e a)
  }

-- | Trait for HTTP basic authentication with the "Basic" scheme.
type BasicAuth = BasicAuth' Required "Basic"

{- | Username for basic authentication. Valid usernames cannot contain
 \':\' characters.
-}
newtype Username = Username ByteString
  deriving newtype (Eq, Ord, Show, Read, IsString)

-- | Password for basic authentication.
newtype Password = Password ByteString
  deriving newtype (Eq, Ord, Show, Read, IsString)

-- | Basic authentication credentials retrieved from an HTTP request
data Credentials = Credentials
  { credentialsUsername :: !Username
  , credentialsPassword :: !Password
  }
  deriving stock (Eq, Ord, Show, Read)

-- | Configuration settings for JWT authentication
data BasicAuthConfig m e a = BasicAuthConfig
  { basicAuthRealm :: Realm
  , toBasicAttribute :: Credentials -> m (Either e a)
  }

-- | Error retrieving basic authentication credentials
data BasicAuthError e
  = BasicAuthHeaderMissing
  | BasicAuthSchemeMismatch
  | BasicAuthCredsBadFormat
  | BasicAuthAttributeError e
  deriving stock (Eq, Show, Read)

instance Trait (BasicAuth' Required scheme m e a) Request where
  type Attribute (BasicAuth' Required scheme m e a) Request = a

instance TraitAbsence (BasicAuth' Required scheme m e a) Request where
  type Absence (BasicAuth' Required scheme m e a) Request = BasicAuthError e

instance Trait (BasicAuth' Optional scheme m e a) Request where
  type Attribute (BasicAuth' Optional scheme m e a) Request = Either (BasicAuthError e) a

instance TraitAbsence (BasicAuth' Optional scheme m e a) Request where
  type Absence (BasicAuth' Optional scheme m e a) Request = Void

basicAuthMiddleware ::
  (Get h (BasicAuth' x scheme m e t) Request, ArrowChoice h) =>
  BasicAuthConfig m e t ->
  h (Linked req Request, Absence (BasicAuth' x scheme m e t) Request) Response ->
  Middleware h req (BasicAuth' x scheme m e t : req)
basicAuthMiddleware BasicAuthConfig{..} errorHandler nextHandler =
  proc request -> do
    result <- probe BasicAuth'{..} -< request
    case result of
      Left err -> errorHandler -< (request, err)
      Right val -> nextHandler -< val

{- | Middleware to add basic authentication protection for a handler.

 Example usage:

 > basicAuth cfg errorHandler nextHandler

 The @errorHandler@ is invoked if the credentials are invalid or
 missing. The @nextHandler@ is invoked if the credentials were
 retrieved successfully.
-}
basicAuth ::
  (Get h (BasicAuth' 'Required "Basic" m e t) Request, ArrowChoice h) =>
  -- | Authentication configuration
  BasicAuthConfig m e t ->
  -- | Error handler
  h (Linked req Request, BasicAuthError e) Response ->
  Middleware h req (BasicAuth m e t : req)
basicAuth = basicAuthMiddleware

{- | Middleware to add optional basic authentication protection for a handler.

 Example usage:

 > optionalBasicAuth cfg nextHandler

 This middleware will not fail if credentials are invalid or
 missing. Instead the trait attribute is of type @'Either'
 ('BasicAuthError' e) t@ so that the handler can process the
 authentication error appropriately.
-}
optionalBasicAuth ::
  (Get h (BasicAuth' 'Optional scheme m e t) Request, ArrowChoice h) =>
  -- | Authentication configuration
  BasicAuthConfig m e t ->
  Middleware h req (BasicAuth' Optional scheme m e t : req)
optionalBasicAuth cfg = basicAuthMiddleware cfg $ arr (absurd . snd)
