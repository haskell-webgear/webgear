{-# LANGUAGE DuplicateRecordFields #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Basic authentication support.
--
module WebGear.Core.Middleware.Auth.Basic
  ( BasicAuth' (..)
  , BasicAuth
  , BasicAuthConfig (..)
  , Realm (..)
  , Username (..)
  , Password (..)
  , Credentials (..)
  , BasicAuthError (..)
  , basicAuth
  , optionalBasicAuth
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

type BasicAuth = BasicAuth' Required "Basic"

-- | Username for basic authentication. Valid usernames cannot contain
-- \':\' characters.
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
  { basicAuthRealm   :: Realm
  , toBasicAttribute :: Credentials -> m (Either e a)
  }

data BasicAuthError e = BasicAuthHeaderMissing
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


basicAuthMiddleware :: (Get h (BasicAuth' x scheme m e t) Request, ArrowChoice h)
                    => BasicAuthConfig m e t
                    -> h (Linked req Request, Absence (BasicAuth' x scheme m e t) Request) Response
                    -> Middleware h req (BasicAuth' x scheme m e t : req)
basicAuthMiddleware BasicAuthConfig{..} errorHandler nextHandler =
  proc request -> do
    result <- probe BasicAuth'{..} -< request
    case result of
      Left err -> errorHandler -< (request, err)
      Right val -> nextHandler -< val

-- | Middleware to add basic authentication protection for a handler.
--
-- Example usage:
--
-- > basicAuth cfg handler
--
-- This middleware returns a 401 response if no credentials are found
-- in the request. It returns a 403 response if credentials are
-- present but 'toBasicAttribute' failed to convert that to value of type
-- t.
basicAuth :: (Get h (BasicAuth' 'Required "Basic" m e t) Request, ArrowChoice h)
          => BasicAuthConfig m e t
          -> h (Linked req Request, BasicAuthError e) Response
          -> Middleware h req (BasicAuth m e t : req)
basicAuth = basicAuthMiddleware

-- | Middleware to add optional basic authentication protection for a handler.
--
-- Example usage:
--
-- > optionalBasicAuth cfg handler
--
-- This middleware will not fail if credentials are invalid or missing
-- in the request. Instead the trait attribute is of type Either
-- 'BasicAuthError' 'Credentials' so that the handler can process the
-- authentication error appropriately.
optionalBasicAuth :: (Get h (BasicAuth' 'Optional scheme m e t) Request, ArrowChoice h)
                  => BasicAuthConfig m e t
                  -> Middleware h req (BasicAuth' Optional scheme m e t : req)
optionalBasicAuth cfg = basicAuthMiddleware cfg $ arr (absurd . snd)
