{-# LANGUAGE DuplicateRecordFields #-}
-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- JWT authentication support.
--
module WebGear.Core.Middleware.Auth.JWT
  ( JWTAuth' (..)
  , JWTAuth
  , JWTAuthConfig (..)
  , Realm (..)
  , JWTAuthError (..)
  , jwtAuth
  , optionalJWTAuth
  , jwtAuth'
  , optionalJWTAuth'
  ) where

import Control.Arrow (ArrowChoice, arr)
import qualified Crypto.JWT as JWT
import Data.Text.Lazy ()
import Data.Void (Void, absurd)
import GHC.TypeLits (Symbol)
import WebGear.Core.Handler (Middleware)
import WebGear.Core.Middleware.Auth.Common (Realm (..))
import WebGear.Core.Modifiers (Existence (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get, Linked, Trait (..), TraitAbsence (..), probe)


-- | Trait for JWT authentication with a bearer token:
-- https://tools.ietf.org/html/rfc6750
--
-- This trait supports a custom scheme instead of the standard
-- "Bearer" scheme.
data JWTAuth' (x :: Existence) (scheme :: Symbol) m e a = JWTAuth'
  { jwtValidationSettings :: JWT.JWTValidationSettings
  , jwkSet                :: JWT.JWKSet
  , toJWTAttribute        :: JWT.ClaimsSet -> m (Either e a)
  }

type JWTAuth = JWTAuth' Required "Bearer"

-- | Configuration settings for JWT authentication
data JWTAuthConfig m e a = JWTAuthConfig
  { jwtAuthRealm          :: Realm
  , jwtValidationSettings :: JWT.JWTValidationSettings
  , jwkSet                :: JWT.JWKSet
  , toJWTAttribute        :: JWT.ClaimsSet -> m (Either e a)
  }

data JWTAuthError e = JWTAuthHeaderMissing
                    | JWTAuthSchemeMismatch
                    | JWTAuthTokenBadFormat JWT.JWTError
                    | JWTAuthAttributeError e
                    deriving stock (Eq, Show)

instance Trait (JWTAuth' Required scheme m e a) Request where
  type Attribute (JWTAuth' Required scheme m e a) Request = a

instance TraitAbsence (JWTAuth' Required scheme m e a) Request where
  type Absence (JWTAuth' Required scheme m e a) Request = JWTAuthError e

instance Trait (JWTAuth' Optional scheme m e a) Request where
  type Attribute (JWTAuth' Optional scheme m e a) Request = Either (JWTAuthError e) a

instance TraitAbsence (JWTAuth' Optional scheme m e a) Request where
  type Absence (JWTAuth' Optional scheme m e a) Request = Void


-- | Middleware to add JWT authentication protection for a
-- handler. Expects the JWT to be available via a standard bearer
-- authorization header in the format:
--
-- > Authorization: Bearer <jwt>
--
-- Example usage:
--
-- > jwtAuth cfg handler
--
-- This middleware returns a 401 response if the authorization header
-- is missing or formatted incorrectly. It returns a 403 response if
-- the JWT is invalid.
jwtAuth :: ( Get h (JWTAuth m e t) Request
           , ArrowChoice h
           )
        => JWTAuthConfig m e t
        -> h (Linked req Request, JWTAuthError e) Response
        -> Middleware h req (JWTAuth m e t : req)
jwtAuth = jwtAuth' @"Bearer"

-- | Middleware to add optional JWT authentication protection for a
-- handler. Expects the JWT to be available via a standard bearer
-- authorization header in the format:
--
-- > Authorization: Bearer <jwt>
--
-- Example usage:
--
-- > optionalJWTAuth cfg handler
--
-- This middleware will not fail if authorization credentials are
-- invalid or missing in the request. Instead the trait attribute is
-- of type Either 'JWTAuthError' 'JWT.ClaimsSet' so that the handler
-- can process the authentication error appropriately.
optionalJWTAuth :: ( Get h (JWTAuth' Optional "Bearer" m e t) Request
                   , ArrowChoice h
                   )
                 => JWTAuthConfig m e t
                 -> Middleware h req (JWTAuth' Optional "Bearer" m e t : req)
optionalJWTAuth = optionalJWTAuth' @"Bearer"

jwtAuthMiddleware :: forall s e t x h m req.
                     ( Get h (JWTAuth' x s m e t) Request
                     , ArrowChoice h
                     )
                  => JWTAuthConfig m e t
                  -> h (Linked req Request, Absence (JWTAuth' x s m e t) Request) Response
                  -> Middleware h req (JWTAuth' x s m e t : req)
jwtAuthMiddleware JWTAuthConfig{..} errorHandler nextHandler =
  proc request -> do
    result <- probe JWTAuth'{..} -< request
    case result of
      Left err -> errorHandler -< (request, err)
      Right val -> nextHandler -< val


-- | Middleware to add JWT authentication protection for a
-- handler. Expects the JWT to be available via an authorization
-- header in the format:
--
-- > Authorization: <scheme> <jwt>
--
-- Example usage:
--
-- > jwtAuth' @"<scheme>" cfg handler
--
-- This middleware returns a 401 response if the authorization header
-- is missing or formatted incorrectly. It returns a 403 response if
-- the JWT is invalid.
jwtAuth' :: forall s e t h m req.
            ( Get h (JWTAuth' Required s m e t) Request
            , ArrowChoice h
            )
         => JWTAuthConfig m e t
         -> h (Linked req Request, JWTAuthError e) Response
         -> Middleware h req (JWTAuth' Required s m e t : req)
jwtAuth' = jwtAuthMiddleware

-- | Middleware to add JWT authentication protection for a
-- handler. Expects the JWT to be available via an authorization
-- header in the format:
--
-- > Authorization: <scheme> <jwt>
--
-- Example usage:
--
-- > optionalJWTAuth' @"<scheme>" cfg handler
--
-- This middleware will not fail if authorization credentials are
-- invalid or missing in the request. Instead the trait attribute is
-- of type Either 'JWTAuthError' 'JWT.ClaimsSet' so that the handler
-- can process the authentication error appropriately.
optionalJWTAuth' :: forall s e t h m req.
                    ( Get h (JWTAuth' Optional s m e t) Request
                    , ArrowChoice h
                    )
                 => JWTAuthConfig m e t
                 -> Middleware h req (JWTAuth' Optional s m e t : req)
optionalJWTAuth' cfg = jwtAuthMiddleware cfg $ arr (absurd . snd)
