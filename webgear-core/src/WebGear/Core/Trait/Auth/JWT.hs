{-# LANGUAGE DuplicateRecordFields #-}

{- | JWT authentication support.

 Middlewares defined in this module add JWT authentication support to
 handlers. In most cases, you just need to use `JWTAuth` trait and
 `jwtAuth` middleware. The table below describes when to use other
 traits and middlewares.

 +----------+-------------+-----------------------+--------------------+
 | Type     | Auth Scheme | Trait                 | Middleware         |
 +----------+-------------+-----------------------+--------------------+
 | Required | Bearer      | 'JWTAuth'             | 'jwtAuth'          |
 +----------+-------------+-----------------------+--------------------+
 | Optional | Bearer      | 'JWTAuth'' 'Optional' | 'optionalJWTAuth'  |
 +----------+-------------+-----------------------+--------------------+
 | Required | Any scheme  | 'JWTAuth'' 'Required' | 'jwtAuth''         |
 +----------+-------------+-----------------------+--------------------+
 | Optional | Any scheme  | 'JWTAuth'' 'Optional' | 'optionalJWTAuth'' |
 +----------+-------------+-----------------------+--------------------+

 For example, given this handler:

 @
 myHandler :: ('Handler' h IO, 'HasTrait' ('JWTAuth' IO () 'JWT.ClaimsSet') req) => 'RequestHandler' h req
 myHandler = ....
 @

 and the following definitions:

 @
 authConfig :: 'JWTAuth' IO () 'JWT.ClaimsSet'
 authConfig = 'JWTAuth''
   { jwtValidationSettings = 'JWT.defaultJWTValidationSettings' (const True)
   , jwkSet = ....
   , toJWTAttribute = pure . Right
   }

 type ErrorTraits = [Status, RequiredHeader \"Content-Type\" Text, RequiredHeader \"WWW-Authenticate\" Text, Body Text]

 errorHandler :: ('Handler' h IO, Sets h ErrorTraits Response)
              => h (Linked req Request, 'JWTAuthError' e) Response
 errorHandler = 'respondUnauthorized' \"Bearer\" \"MyRealm\"
 @

 we can add JWT authentication to @myHandler@:

 @
 myHandlerWithAuth :: ('Handler' h IO, Get h ('JWTAuth' IO () 'JWT.ClaimsSet') Request, Sets h ErrorTraits Response)
                   => 'RequestHandler' h req
 myHandlerWithAuth = 'jwtAuth' authConfig errorHandler myHandler
 @

 The middlewares defined below take a 'JWTAuth'' parameter which has
 settings for validating a JWT. It also contains a function of type
 @'JWT.ClaimsSet' -> m (Either e a)@. This is used to convert the set
 of claims in the JWT to a value of type @a@ or fail with an error of
 type @e@. In this case @a@ is the type of the trait attribute and the
 next handler is invoked after this conversion.

 Middlewares marked as 'Required' take an additional error handling
 arrow as a parameter. This arrow is used when an error is encountered
 in authentication. This arrow receives the original request and a
 'JWTAuthError' as inputs and must produce a response as the output.

 Middlewares marked as 'Optional' do not have this additional error
 handling arrow. Instead, the trait attribute is of type @Either
 ('JWTAuthError' e) a@. The next handler will get the errors in this
 trait attribute and must handle it.
-}
module WebGear.Core.Trait.Auth.JWT (
  JWTAuth' (..),
  JWTAuth,
  Realm (..),
  JWTAuthError (..),
  jwtAuth,
  optionalJWTAuth,
  jwtAuth',
  optionalJWTAuth',
) where

import Control.Arrow (ArrowChoice, arr)
import qualified Crypto.JWT as JWT
import Data.Void (Void, absurd)
import GHC.TypeLits (Symbol)
import WebGear.Core.Handler
import WebGear.Core.Modifiers (Documentation, Existence (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait
import WebGear.Core.Trait.Auth.Common

{- | Trait for JWT authentication with a bearer token:
 https://tools.ietf.org/html/rfc6750

 This trait supports a custom scheme instead of the standard
 \"Bearer\" scheme.
-}
data JWTAuth' (x :: Existence) (scheme :: Symbol) m e a = JWTAuth'
  { -- | Settings to validate the JWT
    jwtValidationSettings :: JWT.JWTValidationSettings
  , -- | JWK to validate the JWT
    jwkSet :: JWT.JWKSet
  , -- | Convert the claims set to the trait attribute or an error
    toJWTAttribute :: JWT.ClaimsSet -> m (Either e a)
  , -- | Documentation for the authentication scheme
    authDocumentation :: Documentation
  }

-- | Trait for JWT authentication with the \"Bearer\" scheme
type JWTAuth = JWTAuth' Required "Bearer"

-- | Error extracting a JWT from a request
data JWTAuthError e
  = JWTAuthHeaderMissing
  | JWTAuthSchemeMismatch
  | JWTAuthTokenBadFormat JWT.JWTError
  | JWTAuthAttributeError e
  deriving stock (Eq, Show)

instance WebGear.Core.Trait.Trait (JWTAuth' Required scheme m e a) Request where
  type Attribute (JWTAuth' Required scheme m e a) Request = a

instance WebGear.Core.Trait.TraitAbsence (JWTAuth' Required scheme m e a) Request where
  type Absence (JWTAuth' Required scheme m e a) Request = JWTAuthError e

instance WebGear.Core.Trait.Trait (JWTAuth' Optional scheme m e a) Request where
  type Attribute (JWTAuth' Optional scheme m e a) Request = Either (JWTAuthError e) a

instance WebGear.Core.Trait.TraitAbsence (JWTAuth' Optional scheme m e a) Request where
  type Absence (JWTAuth' Optional scheme m e a) Request = Void

{- | Middleware to add JWT authentication protection for a
 handler. Expects the JWT to be available via a standard bearer
 authorization header in the format:

 > Authorization: Bearer <jwt>

 Example usage:

 > jwtAuth cfg errorHandler nextHandler

 The @errorHandler@ is invoked if the credentials are invalid or
 missing. The @nextHandler@ is invoked if the credentials were
 retrieved successfully.
-}
jwtAuth ::
  ( WebGear.Core.Trait.Get h (JWTAuth m e t) Request
  , ArrowChoice h
  ) =>
  -- | Authentication configuration
  JWTAuth m e t ->
  -- | Error handler
  h (WebGear.Core.Trait.Linked req Request, JWTAuthError e) Response ->
  Middleware h req (JWTAuth m e t : req)
jwtAuth = jwtAuth' @"Bearer"

{- | Middleware to add optional JWT authentication protection for a
 handler. Expects the JWT to be available via a standard bearer
 authorization header in the format:

 > Authorization: Bearer <jwt>

 Example usage:

 > optionalJWTAuth cfg handler

 This middleware will not fail if authorization credentials are
 invalid or missing. Instead the trait attribute is of type @'Either'
 ('JWTAuthError' e) t@ so that the handler can process the
 authentication error appropriately.
-}
optionalJWTAuth ::
  ( WebGear.Core.Trait.Get h (JWTAuth' Optional "Bearer" m e t) Request
  , ArrowChoice h
  ) =>
  -- | Authentication configuration
  JWTAuth' Optional "Bearer" m e t ->
  Middleware h req (JWTAuth' Optional "Bearer" m e t : req)
optionalJWTAuth = optionalJWTAuth' @"Bearer"

jwtAuthMiddleware ::
  forall s e t x h m req.
  ( WebGear.Core.Trait.Get h (JWTAuth' x s m e t) Request
  , ArrowChoice h
  ) =>
  JWTAuth' x s m e t ->
  h (WebGear.Core.Trait.Linked req Request, WebGear.Core.Trait.Absence (JWTAuth' x s m e t) Request) Response ->
  Middleware h req (JWTAuth' x s m e t : req)
jwtAuthMiddleware authCfg errorHandler nextHandler =
  proc request -> do
    result <- WebGear.Core.Trait.probe authCfg -< request
    case result of
      Left err -> errorHandler -< (request, err)
      Right val -> nextHandler -< val

{- | Middleware to add JWT authentication protection for a
 handler. Expects the JWT to be available via an authorization header
 in the format:

 > Authorization: <scheme> <jwt>

 Example usage:

 > jwtAuth' @"<scheme>" cfg errorHandler nextHandler

 The @errorHandler@ is invoked if the credentials are invalid or
 missing. The @nextHandler@ is invoked if the credentials were
 retrieved successfully.
-}
jwtAuth' ::
  forall s e t h m req.
  ( WebGear.Core.Trait.Get h (JWTAuth' Required s m e t) Request
  , ArrowChoice h
  ) =>
  -- | Authentication configuration
  JWTAuth' Required s m e t ->
  -- | Error handler
  h (WebGear.Core.Trait.Linked req Request, JWTAuthError e) Response ->
  Middleware h req (JWTAuth' Required s m e t : req)
jwtAuth' = jwtAuthMiddleware

{- | Middleware to add JWT authentication protection for a
 handler. Expects the JWT to be available via an authorization header
 in the format:

 > Authorization: <scheme> <jwt>

 Example usage:

 > optionalJWTAuth' @"<scheme>" cfg nextHandler

 This middleware will not fail if authorization credentials are
 invalid or missing. Instead the trait attribute is of type @'Either'
 ('JWTAuthError' e) t@ so that the handler can process the
 authentication error appropriately.
-}
optionalJWTAuth' ::
  forall s e t h m req.
  ( WebGear.Core.Trait.Get h (JWTAuth' Optional s m e t) Request
  , ArrowChoice h
  ) =>
  -- | Authentication configuration
  JWTAuth' Optional s m e t ->
  Middleware h req (JWTAuth' Optional s m e t : req)
optionalJWTAuth' cfg = jwtAuthMiddleware cfg $ arr (absurd . snd)
