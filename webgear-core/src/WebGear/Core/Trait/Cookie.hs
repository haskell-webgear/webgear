-- | Traits and middlewares to handle cookies in requests and responses.
module WebGear.Core.Trait.Cookie (
  -- * Traits
  Cookie (..),
  CookieNotFound (..),
  CookieParseError (..),
  SetCookie (..),

  -- * Middlewares
  cookie,
  optionalCookie,
  setCookie,
  setOptionalCookie,
) where

import Control.Arrow (ArrowChoice)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.TypeLits (Symbol)
import qualified Web.Cookie as Cookie
import WebGear.Core.Handler (Middleware)
import WebGear.Core.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (
  Get,
  HasTrait,
  Prerequisite,
  Set,
  Trait (..),
  TraitAbsence (..),
  With,
  plant,
  probe,
 )
import WebGear.Core.Trait.Header (RequestHeader)

-- | Indicates a missing cookie
data CookieNotFound = CookieNotFound
  deriving stock (Read, Show, Eq)

-- | Error in converting a cookie to the expected type
newtype CookieParseError = CookieParseError Text
  deriving stock (Read, Show, Eq)

-- | Trait for a cookie in HTTP requests
data Cookie (e :: Existence) (name :: Symbol) (val :: Type) = Cookie

instance Trait (Cookie Required name val) Request where
  type Attribute (Cookie Required name val) Request = val

type instance
  Prerequisite (Cookie e name val) ts Request =
    HasTrait (RequestHeader e Strict "Cookie" Text) ts

instance TraitAbsence (Cookie Required name val) Request where
  type Absence (Cookie Required name val) Request = Either CookieNotFound CookieParseError

instance Trait (Cookie Optional name val) Request where
  type Attribute (Cookie Optional name val) Request = Maybe val

instance TraitAbsence (Cookie Optional name val) Request where
  type Absence (Cookie Optional name val) Request = CookieParseError

cookieHandler ::
  forall name val e h ts.
  ( ArrowChoice h
  , Get h (Cookie e name val) Request
  , HasTrait (RequestHeader e Strict "Cookie" Text) ts
  ) =>
  -- | error handler
  h (Request `With` ts, Absence (Cookie e name val) Request) Response ->
  Middleware h ts (Cookie e name val : ts)
cookieHandler errorHandler nextHandler = proc request -> do
  result <- probe Cookie -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right val -> nextHandler -< val
{-# INLINE cookieHandler #-}

{- | Extract a cookie and convert it to a value of type @val@.

 The associated trait attribute has type @val@.

 Example usage:

 > cookie @"name" @Integer errorHandler okHandler
-}
cookie ::
  forall name val h ts.
  ( ArrowChoice h
  , Get h (Cookie Required name val) Request
  , HasTrait (RequestHeader Required Strict "Cookie" Text) ts
  ) =>
  -- | Error handler
  h (Request `With` ts, Either CookieNotFound CookieParseError) Response ->
  Middleware h ts (Cookie Required name val : ts)
cookie = cookieHandler
{-# INLINE cookie #-}

{- | Extract an optional cookie and convert it to a value of type @val@.

 The associated trait attribute has type @Maybe val@; a @Nothing@
 value indicates that the cookie is missing from the request.

 Example usage:

 > optionalCookie @"name" @Integer errorHandler okHandler
-}
optionalCookie ::
  forall name val h ts.
  ( ArrowChoice h
  , Get h (Cookie Optional name val) Request
  , HasTrait (RequestHeader Optional Strict "Cookie" Text) ts
  ) =>
  -- | Error handler
  h (Request `With` ts, CookieParseError) Response ->
  Middleware h ts (Cookie Optional name val : ts)
optionalCookie = cookieHandler
{-# INLINE optionalCookie #-}

-- | Trait for a cookie in HTTP responses
data SetCookie (e :: Existence) (name :: Symbol) = SetCookie

instance Trait (SetCookie Required name) Response where
  type Attribute (SetCookie Required name) Response = Cookie.SetCookie

instance Trait (SetCookie Optional name) Response where
  type Attribute (SetCookie Optional name) Response = Maybe Cookie.SetCookie

{- | Set a cookie value in a response.

 Example usage:

 > response' <- setCookie @"name" -< (response, cookie)
-}
setCookie ::
  forall name h ts.
  (Set h (SetCookie Required name) Response) =>
  h (Response `With` ts, Cookie.SetCookie) (Response `With` (SetCookie Required name : ts))
setCookie = plant SetCookie
{-# INLINE setCookie #-}

{- | Set an optional cookie value in a response.

 Setting the cookie to 'Nothing' will remove it from the response if
 it was previously set. The cookie will be considered as optional in
 all relevant places (such as documentation).

 Example usage:

 > response' <- setOptionalCookie @"name" -< (response, cookie)
-}
setOptionalCookie ::
  forall name h ts.
  (Set h (SetCookie Optional name) Response) =>
  h (Response `With` ts, Maybe Cookie.SetCookie) (Response `With` (SetCookie Optional name : ts))
setOptionalCookie = plant SetCookie
{-# INLINE setOptionalCookie #-}
