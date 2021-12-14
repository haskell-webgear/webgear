{- |
 Handle request headers
-}
module WebGear.Core.Trait.Header (
  -- * Traits
  Header (..),
  HeaderNotFound (..),
  HeaderParseError (..),
  RequiredHeader,
  OptionalHeader,

  -- * Middlewares
  header,
  optionalHeader,
  lenientHeader,
  optionalLenientHeader,
  setHeader,
  setOptionalHeader,
) where

import Control.Arrow (ArrowChoice, arr)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.TypeLits (Symbol)
import WebGear.Core.Handler (Middleware)
import WebGear.Core.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get (..), Linked, Set, Trait (..), TraitAbsence (..), plant, probe)

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
data Header (e :: Existence) (p :: ParseStyle) (name :: Symbol) (val :: Type) = Header

-- | A `Header` that is required and parsed strictly
type RequiredHeader = Header Required Strict

-- | A `Header` that is optional and parsed strictly
type OptionalHeader = Header Optional Strict

instance Trait (Header Required Strict name val) Request where
  type Attribute (Header Required Strict name val) Request = val

instance TraitAbsence (Header Required Strict name val) Request where
  type Absence (Header Required Strict name val) Request = Either HeaderNotFound HeaderParseError

instance Trait (Header Optional Strict name val) Request where
  type Attribute (Header Optional Strict name val) Request = Maybe val

instance TraitAbsence (Header Optional Strict name val) Request where
  type Absence (Header Optional Strict name val) Request = HeaderParseError

instance Trait (Header Required Lenient name val) Request where
  type Attribute (Header Required Lenient name val) Request = Either Text val

instance TraitAbsence (Header Required Lenient name val) Request where
  type Absence (Header Required Lenient name val) Request = HeaderNotFound

instance Trait (Header Optional Lenient name val) Request where
  type Attribute (Header Optional Lenient name val) Request = Maybe (Either Text val)

instance TraitAbsence (Header Optional Lenient name val) Request where
  type Absence (Header Optional Lenient name val) Request = Void

headerHandler ::
  forall name val e p h req.
  (Get h (Header e p name val) Request, ArrowChoice h) =>
  -- | error handler
  h (Linked req Request, Absence (Header e p name val) Request) Response ->
  Middleware h req (Header e p name val : req)
headerHandler errorHandler nextHandler = proc request -> do
  result <- probe Header -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right val -> nextHandler -< val

{- | Extract a header value and convert it to a value of type @val@.

 The associated trait attribute has type @val@.

 Example usage:

 > header @"Content-Length" @Integer errorHandler okHandler
-}
header ::
  forall name val h req.
  (Get h (Header Required Strict name val) Request, ArrowChoice h) =>
  h (Linked req Request, Either HeaderNotFound HeaderParseError) Response ->
  Middleware h req (Header Required Strict name val : req)
header = headerHandler

{- | Extract an optional header value and convert it to a value of type
 @val@.

 The associated trait attribute has type @Maybe val@; a @Nothing@
 value indicates that the header is missing from the request.

 Example usage:

 > optionalHeader @"Content-Length" @Integer errorHandler okHandler
-}
optionalHeader ::
  forall name val h req.
  (Get h (Header Optional Strict name val) Request, ArrowChoice h) =>
  h (Linked req Request, HeaderParseError) Response ->
  Middleware h req (Header Optional Strict name val : req)
optionalHeader = headerHandler

{- | Extract a header value and convert it to a value of type @val@.

 The associated trait attribute has type @Either Text val@. The
 parsing is done leniently and any errors are reported in the trait
 attribute.

 Example usage:

 > lenientHeader @"Content-Length" @Integer errorHandler okHandler
-}
lenientHeader ::
  forall name val h req.
  (Get h (Header Required Lenient name val) Request, ArrowChoice h) =>
  h (Linked req Request, HeaderNotFound) Response ->
  Middleware h req (Header Required Lenient name val : req)
lenientHeader = headerHandler

{- | Extract a header value and convert it to a value of type @val@.

 The associated trait attribute has type @Maybe (Either Text
 val)@. The parsing is done leniently. Any parsing errors and
 missing header are reported in the trait attribute.

 Example usage:

 > optionalLenientHeader @"Content-Length" @Integer handler
-}
optionalLenientHeader ::
  forall name val h req.
  (Get h (Header Optional Lenient name val) Request, ArrowChoice h) =>
  Middleware h req (Header Optional Lenient name val : req)
optionalLenientHeader = headerHandler $ arr (absurd . snd)

instance Trait (Header Required Strict name val) Response where
  type Attribute (Header Required Strict name val) Response = val

instance Trait (Header Optional Strict name val) Response where
  type Attribute (Header Optional Strict name val) Response = Maybe val

{- | Set a header value in a response.

 Example usage:

 > setHeader @"Content-Length"
-}
setHeader ::
  forall name val h res.
  Set h (Header Required Strict name val) Response =>
  h (Linked res Response, val) (Linked (Header Required Strict name val : res) Response)
setHeader = plant Header

{- | Set an optional header value in a response.

 Example usage:

 > setHeader "Content-Length" @Integer
-}
setOptionalHeader ::
  forall name val h res.
  Set h (Header Optional Strict name val) Response =>
  h (Linked res Response, Maybe val) (Linked (Header Optional Strict name val : res) Response)
setOptionalHeader = plant Header
