{-# LANGUAGE DuplicateRecordFields #-}

{- | HTTP basic authentication support.

 Middlewares defined in this module add basic authentication support
 to handlers. In most cases, you just need to use `BasicAuth` trait
 and `basicAuth` middleware. The table below describes when to use
 other traits and middlewares.

 +----------+-------------+-------------------------+----------------------+
 | Type     | Auth Scheme | Trait                   | Middleware           |
 +----------+-------------+-------------------------+----------------------+
 | Required | Basic       | 'BasicAuth'             | 'basicAuth'          |
 +----------+-------------+-------------------------+----------------------+
 | Optional | Basic       | 'BasicAuth'' 'Optional' | 'optionalBasicAuth'  |
 +----------+-------------+-------------------------+----------------------+
 | Required | Any scheme  | 'BasicAuth'' 'Required' | 'basicAuth''         |
 +----------+-------------+-------------------------+----------------------+
 | Optional | Any scheme  | 'BasicAuth'' 'Optional' | 'optionalBasicAuth'' |
 +----------+-------------+-------------------------+----------------------+

 For example, given this handler:

 @
 myHandler :: ('Handler' h IO, 'HasTrait' ('BasicAuth' IO () 'Credentials') req) => 'RequestHandler' h req
 myHandler = ....
 @

 and the following definitions:

 @
 authConfig :: 'BasicAuth' IO () 'Credentials'
 authConfig = 'BasicAuth'' { toBasicAttribute = pure . Right }

 type ErrorTraits = [Status, RequiredHeader \"Content-Type\" Text, RequiredHeader \"WWW-Authenticate\" Text, Body Text]

 errorHandler :: ('Handler' h IO, Sets h ErrorTraits Response)
              => h (Linked req Request, 'BasicAuthError' e) Response
 errorHandler = 'respondUnauthorized' \"Basic\" \"MyRealm\"
 @

 we can add basic authentication to @myHandler@:

 @
 myHandlerWithAuth :: ('Handler' h IO, Get h ('BasicAuth' IO () 'Credentials') Request, Sets h ErrorTraits Response)
                   => 'RequestHandler' h req
 myHandlerWithAuth = 'basicAuth' authConfig errorHandler myHandler
 @

 The middlewares defined below take a 'BasicAuth'' parameter which is
 a newtype wrapper over a function of type @'Credentials' -> m (Either
 e a)@. This is used to convert the user supplied credentials to a
 value of type @a@ or fail with an error of type @e@. The next handler
 is invoked after this conversion and can access @a@ as a trait
 attribute.

 Middlewares marked as 'Required' take an additional error handling
 arrow as a parameter. This arrow is used when an error is encountered
 in authentication. This arrow receives the original request and a
 'BasicAuthError' as inputs and must produce a response as the output.

 Middlewares marked as 'Optional' do not have this additional error
 handling arrow. Instead, the trait attribute is of type @Either
 ('BasicAuthError' e) a@. The next handler will get the errors in this
 trait attribute and must handle it.
-}
module WebGear.Core.Trait.Auth.Basic (
  BasicAuth' (..),
  BasicAuth,
  Realm (..),
  Username (..),
  Password (..),
  Credentials (..),
  BasicAuthError (..),
  basicAuth,
  basicAuth',
  optionalBasicAuth,
  optionalBasicAuth',
) where

import Control.Arrow (ArrowChoice, arr)
import Data.ByteString (ByteString)
import Data.String (IsString)
import Data.Void (Void, absurd)
import GHC.TypeLits (Symbol)
import WebGear.Core.Handler
import WebGear.Core.Modifiers (Documentation, Existence (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait
import WebGear.Core.Trait.Auth.Common

-- | Trait for HTTP basic authentication: https://tools.ietf.org/html/rfc7617
data BasicAuth' (x :: Existence) (scheme :: Symbol) m e a = BasicAuth'
  { -- | Convert the credentials to the trait attribute or an error
    toBasicAttribute :: Credentials -> m (Either e a)
  , -- | Documentation for the authentication scheme
    authDocumentation :: Documentation
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
  BasicAuth' x scheme m e t ->
  h (Linked req Request, Absence (BasicAuth' x scheme m e t) Request) Response ->
  Middleware h req (BasicAuth' x scheme m e t : req)
basicAuthMiddleware authCfg errorHandler nextHandler =
  proc request -> do
    result <- probe authCfg -< request
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
  forall m e t h req.
  (Get h (BasicAuth' Required "Basic" m e t) Request, ArrowChoice h) =>
  -- | Authentication configuration
  BasicAuth m e t ->
  -- | Error handler
  h (Linked req Request, BasicAuthError e) Response ->
  Middleware h req (BasicAuth m e t : req)
basicAuth = basicAuth'

{- | Similar to `basicAuth` but supports a custom authentication scheme.

 Example usage:

 > basicAuth' @"scheme" cfg errorHandler nextHandler
-}
basicAuth' ::
  forall scheme m e t h req.
  (Get h (BasicAuth' Required scheme m e t) Request, ArrowChoice h) =>
  -- | Authentication configuration
  BasicAuth' Required scheme m e t ->
  -- | Error handler
  h (Linked req Request, BasicAuthError e) Response ->
  Middleware h req (BasicAuth' Required scheme m e t : req)
basicAuth' = basicAuthMiddleware

{- | Middleware to add optional basic authentication protection for a handler.

 Example usage:

 > optionalBasicAuth cfg nextHandler

 This middleware will not fail if credentials are invalid or
 missing. Instead the trait attribute is of type @'Either'
 ('BasicAuthError' e) t@ so that the handler can process the
 authentication error appropriately.
-}
optionalBasicAuth ::
  forall m e t h req.
  (Get h (BasicAuth' Optional "Basic" m e t) Request, ArrowChoice h) =>
  -- | Authentication configuration
  BasicAuth' Optional "Basic" m e t ->
  Middleware h req (BasicAuth' Optional "Basic" m e t : req)
optionalBasicAuth = optionalBasicAuth'

{- | Similar to `optionalBasicAuth` but supports a custom authentication
   scheme.

 Example usage:

 > optionalBasicAuth' @"scheme" cfg nextHandler
-}
optionalBasicAuth' ::
  forall scheme m e t h req.
  (Get h (BasicAuth' Optional scheme m e t) Request, ArrowChoice h) =>
  -- | Authentication configuration
  BasicAuth' Optional scheme m e t ->
  Middleware h req (BasicAuth' Optional scheme m e t : req)
optionalBasicAuth' cfg = basicAuthMiddleware cfg $ arr (absurd . snd)
