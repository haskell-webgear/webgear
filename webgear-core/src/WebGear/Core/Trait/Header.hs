{- | Traits and middlewares to handle request and response headers.

 There are a number of ways to extract a header value from a request:

 The `header` middleware can extract a header value trait and invoke
 another handler. An error handler is invoked if the header is missing
 or the parsing fails.

 The `optionalHeader` middleware is similar but will not invoke the
 error handling in case the header is missing. Instead, the trait
 value will be set to `Nothing` in that case.

 The `lenientHeader` middleware requires the header to be present. But
 the trait attribute will be set to 'Left' @msg@ if an error occurs
 while parsing it to a Haskell value. Here @msg@ will indicate the
 error in parsing.

 Finally, we have `optionalLenientHeader` which combines the behaviors
 of `optionalHeader` and `lenientHeader`. In this case, the header
 extraction never fails. Missing headers and parse errors are
 indicated in the trait attribute passed to next handler.

 A response header can be set using `setHeader` or `setOptionalHeader`
 arrows. They accept a witnessed response and a header value and sets
 the header in the response. You can generate an input response object
 using functions from "WebGear.Core.Trait.Status" module.
-}
module WebGear.Core.Trait.Header (
  -- * Traits
  RequestHeader (..),
  HeaderNotFound (..),
  HeaderParseError (..),
  RequiredRequestHeader,
  OptionalRequestHeader,
  ResponseHeader (..),
  RequiredResponseHeader,
  OptionalResponseHeader,

  -- * Middlewares
  header,
  optionalHeader,
  lenientHeader,
  optionalLenientHeader,
  setHeader,
  setOptionalHeader,
  acceptMatch,
) where

import Control.Arrow (ArrowChoice, arr)
import Control.Arrow.Operations (ArrowError)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.TypeLits (Symbol)
import qualified Network.HTTP.Media as HTTP
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Middleware, RouteMismatch, routeMismatch)
import WebGear.Core.MIMETypes (MIMEType (..))
import WebGear.Core.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Core.Request (Request, requestHeader)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (
  Get (..),
  Prerequisite,
  Set,
  Trait (..),
  TraitAbsence (..),
  With,
  plant,
  probe,
  unwitness,
 )

-- | Indicates a missing header
data HeaderNotFound = HeaderNotFound
  deriving stock (Read, Show, Eq)

-- | Error in converting a header
newtype HeaderParseError = HeaderParseError Text
  deriving stock (Read, Show, Eq)

{- | A 'Trait' for capturing an HTTP header of specified @name@ and
 converting it to some type @val@. The modifiers @e@ and @p@ determine
 how missing headers and parsing errors are handled. The header name
 is compared case-insensitively.
-}
data RequestHeader (e :: Existence) (p :: ParseStyle) (name :: Symbol) (val :: Type) = RequestHeader

-- | A `RequestHeader` that is required and parsed strictly
type RequiredRequestHeader = RequestHeader Required Strict

-- | A `RequestHeader` that is optional and parsed strictly
type OptionalRequestHeader = RequestHeader Optional Strict

instance Trait (RequestHeader Required Strict name val) Request where
  type Attribute (RequestHeader Required Strict name val) Request = val

instance TraitAbsence (RequestHeader Required Strict name val) Request where
  type Absence (RequestHeader Required Strict name val) Request = Either HeaderNotFound HeaderParseError

instance Trait (RequestHeader Optional Strict name val) Request where
  type Attribute (RequestHeader Optional Strict name val) Request = Maybe val

instance TraitAbsence (RequestHeader Optional Strict name val) Request where
  type Absence (RequestHeader Optional Strict name val) Request = HeaderParseError

instance Trait (RequestHeader Required Lenient name val) Request where
  type Attribute (RequestHeader Required Lenient name val) Request = Either Text val

instance TraitAbsence (RequestHeader Required Lenient name val) Request where
  type Absence (RequestHeader Required Lenient name val) Request = HeaderNotFound

instance Trait (RequestHeader Optional Lenient name val) Request where
  type Attribute (RequestHeader Optional Lenient name val) Request = Maybe (Either Text val)

instance TraitAbsence (RequestHeader Optional Lenient name val) Request where
  type Absence (RequestHeader Optional Lenient name val) Request = Void

type instance Prerequisite (RequestHeader e p name val) ts Request = ()

headerHandler ::
  forall name val e p h ts.
  (Get h (RequestHeader e p name val) Request, ArrowChoice h) =>
  -- | error handler
  h (Request `With` ts, Absence (RequestHeader e p name val) Request) Response ->
  Middleware h ts (RequestHeader e p name val : ts)
headerHandler errorHandler nextHandler = proc request -> do
  result <- probe RequestHeader -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right val -> nextHandler -< val
{-# INLINE headerHandler #-}

{- | Extract a header value and convert it to a value of type @val@.

 The associated trait attribute has type @val@.

 Example usage:

 > header @"Content-Length" @Integer errorHandler okHandler
-}
header ::
  forall name val h ts.
  (Get h (RequestHeader Required Strict name val) Request, ArrowChoice h) =>
  -- | Error handler
  h (Request `With` ts, Either HeaderNotFound HeaderParseError) Response ->
  Middleware h ts (RequestHeader Required Strict name val : ts)
header = headerHandler
{-# INLINE header #-}

{- | Extract an optional header value and convert it to a value of type
 @val@.

 The associated trait attribute has type @Maybe val@; a @Nothing@
 value indicates that the header is missing from the request.

 Example usage:

 > optionalHeader @"Content-Length" @Integer errorHandler okHandler
-}
optionalHeader ::
  forall name val h ts.
  (Get h (RequestHeader Optional Strict name val) Request, ArrowChoice h) =>
  -- | Error handler
  h (Request `With` ts, HeaderParseError) Response ->
  Middleware h ts (RequestHeader Optional Strict name val : ts)
optionalHeader = headerHandler
{-# INLINE optionalHeader #-}

{- | Extract a header value and convert it to a value of type @val@.

 The associated trait attribute has type @Either Text val@. The
 parsing is done leniently and any errors are reported in the trait
 attribute.

 Example usage:

 > lenientHeader @"Content-Length" @Integer errorHandler okHandler
-}
lenientHeader ::
  forall name val h ts.
  (Get h (RequestHeader Required Lenient name val) Request, ArrowChoice h) =>
  -- | Error handler
  h (Request `With` ts, HeaderNotFound) Response ->
  Middleware h ts (RequestHeader Required Lenient name val : ts)
lenientHeader = headerHandler
{-# INLINE lenientHeader #-}

{- | Extract a header value and convert it to a value of type @val@.

 The associated trait attribute has type @Maybe (Either Text
 val)@. The parsing is done leniently. Any parsing errors and
 missing header are reported in the trait attribute.

 Example usage:

 > optionalLenientHeader @"Content-Length" @Integer handler
-}
optionalLenientHeader ::
  forall name val h ts.
  (Get h (RequestHeader Optional Lenient name val) Request, ArrowChoice h) =>
  Middleware h ts (RequestHeader Optional Lenient name val : ts)
optionalLenientHeader = headerHandler $ arr (absurd . snd)
{-# INLINE optionalLenientHeader #-}

{- | A 'Trait' for setting a header in the HTTP response. It has a
 specified @name@ and a value of type @val@. The header name is
 compared case-insensitively. The modifier @e@ determines whether the
 header is mandatory or optional.
-}
data ResponseHeader (e :: Existence) (name :: Symbol) (val :: Type) = ResponseHeader

-- | A `ResponseHeader` that is required
type RequiredResponseHeader = ResponseHeader Required

-- | A `ResponseHeader` that is optional
type OptionalResponseHeader = ResponseHeader Optional

instance Trait (ResponseHeader Required name val) Response where
  type Attribute (ResponseHeader Required name val) Response = val

instance Trait (ResponseHeader Optional name val) Response where
  type Attribute (ResponseHeader Optional name val) Response = Maybe val

{- | Set a header value in a response.

 Example usage:

 > response' <- setHeader @"Content-Length" -< (response, 42)
-}
setHeader ::
  forall name val h ts.
  (Set h (ResponseHeader Required name val) Response) =>
  h (Response `With` ts, val) (Response `With` (ResponseHeader Required name val : ts))
setHeader = plant ResponseHeader
{-# INLINE setHeader #-}

{- | Set an optional header value in a response.

 Setting the header to 'Nothing' will remove it from the response if
 it was previously set. The header will be considered as optional in
 all relevant places (such as documentation).

 Example usage:

 > response' <- setOptionalHeader @"Content-Length" -< (response, Just 42)
-}
setOptionalHeader ::
  forall name val h ts.
  (Set h (ResponseHeader Optional name val) Response) =>
  h (Response `With` ts, Maybe val) (Response `With` (ResponseHeader Optional name val : ts))
setOptionalHeader = plant ResponseHeader
{-# INLINE setOptionalHeader #-}

{- | Match the Accept header of the incoming request with the specified mediatype. Another handler will be tried
 if the match fails.

 Example usage:

 @
   acceptMatch 'WebGear.Core.MIMETypes.JSON' handler
 @
-}
acceptMatch :: (ArrowChoice h, ArrowError RouteMismatch h, MIMEType mt) => mt -> Middleware h ts ts
acceptMatch mt nextHandler =
  proc request -> do
    let acceptHeader = fromMaybe "*/*" $ requestHeader HTTP.hAccept $ unwitness request
    case HTTP.matchAccept [mimeType mt] acceptHeader of
      Just _ -> nextHandler -< request
      Nothing -> routeMismatch -< ()
