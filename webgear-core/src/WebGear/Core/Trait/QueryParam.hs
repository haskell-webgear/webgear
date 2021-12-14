{- |
 Handle query parameters
-}
module WebGear.Core.Trait.QueryParam (
  -- * Traits
  QueryParam (..),
  RequiredQueryParam,
  OptionalQueryParam,
  ParamNotFound (..),
  ParamParseError (..),

  -- * Middlewares
  queryParam,
  optionalQueryParam,
  lenientQueryParam,
  optionalLenientQueryParam,
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
import WebGear.Core.Trait (Get, Linked, Trait (..), TraitAbsence (..), probe)

{- | Capture a query parameter with a specified @name@ and convert it to
 a value of type @val@. The type parameter @e@ denotes whether the
 query parameter is required to be present. The parse style parameter
 @p@ determines whether the conversion is applied strictly or
 leniently.
-}
data QueryParam (e :: Existence) (p :: ParseStyle) (name :: Symbol) (val :: Type) = QueryParam

-- | `QueryParam` that is required and parsed strictly
type RequiredQueryParam = QueryParam Required Strict

-- | `QueryParam` that is optional and parsed strictly
type OptionalQueryParam = QueryParam Optional Strict

-- | Indicates a missing query parameter
data ParamNotFound = ParamNotFound
  deriving stock (Read, Show, Eq)

-- | Error in converting a query parameter
newtype ParamParseError = ParamParseError Text
  deriving stock (Read, Show, Eq)

instance Trait (QueryParam Required Strict name val) Request where
  type Attribute (QueryParam Required Strict name val) Request = val

instance TraitAbsence (QueryParam Required Strict name val) Request where
  type Absence (QueryParam Required Strict name val) Request = Either ParamNotFound ParamParseError

instance Trait (QueryParam Optional Strict name val) Request where
  type Attribute (QueryParam Optional Strict name val) Request = Maybe val

instance TraitAbsence (QueryParam Optional Strict name val) Request where
  type Absence (QueryParam Optional Strict name val) Request = ParamParseError

instance Trait (QueryParam Required Lenient name val) Request where
  type Attribute (QueryParam Required Lenient name val) Request = Either Text val

instance TraitAbsence (QueryParam Required Lenient name val) Request where
  type Absence (QueryParam Required Lenient name val) Request = ParamNotFound

instance Trait (QueryParam Optional Lenient name val) Request where
  type Attribute (QueryParam Optional Lenient name val) Request = Maybe (Either Text val)

instance TraitAbsence (QueryParam Optional Lenient name val) Request where
  type Absence (QueryParam Optional Lenient name val) Request = Void

queryParamHandler ::
  forall name val e p h req.
  (Get h (QueryParam e p name val) Request, ArrowChoice h) =>
  h (Linked req Request, Absence (QueryParam e p name val) Request) Response ->
  Middleware h req (QueryParam e p name val : req)
queryParamHandler errorHandler nextHandler = proc request -> do
  result <- probe QueryParam -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right val -> nextHandler -< val

{- | Extract a query parameter and convert it to a value of type
 @val@. The associated trait attribute has type @val@.

 Example usage:

 > queryParam @"limit" @Integer errorHandler okHandler
-}
queryParam ::
  forall name val h req.
  (Get h (QueryParam Required Strict name val) Request, ArrowChoice h) =>
  h (Linked req Request, Either ParamNotFound ParamParseError) Response ->
  Middleware h req (QueryParam Required Strict name val : req)
queryParam = queryParamHandler

{- | Extract an optional query parameter and convert it to a value of
 type @val@. The associated trait attribute has type @Maybe val@; a
 @Nothing@ value indicates that the parameter is missing from the
 request.

 Example usage:

 > optionalQueryParam @"limit" @Integer errorHandler okHandler
-}
optionalQueryParam ::
  forall name val h req.
  (Get h (QueryParam Optional Strict name val) Request, ArrowChoice h) =>
  h (Linked req Request, ParamParseError) Response ->
  Middleware h req (QueryParam Optional Strict name val : req)
optionalQueryParam = queryParamHandler

{- | Extract a query parameter and convert it to a value of type @val@.

 The associated trait attribute has type @Either Text val@. The
 parsing is done leniently and any errors are reported in the trait
 attribute.

 Example usage:

 > lenientQueryParam @"limit" @Integer errorHandler okHandler
-}
lenientQueryParam ::
  forall name val h req.
  (Get h (QueryParam Required Lenient name val) Request, ArrowChoice h) =>
  h (Linked req Request, ParamNotFound) Response ->
  Middleware h req (QueryParam Required Lenient name val : req)
lenientQueryParam = queryParamHandler

{- | Extract a query parameter and convert it to a value of type @val@.

 Example usage:

 > optionalLenientQueryParam @"Content-Length" @Integer handler

 The associated trait attribute has type @Maybe (Either Text
 val)@. The parsing is done leniently. Any parsing errors and
 missing query parameters are reported in the trait attribute.
-}
optionalLenientQueryParam ::
  forall name val h req.
  (Get h (QueryParam Optional Lenient name val) Request, ArrowChoice h) =>
  Middleware h req (QueryParam Optional Lenient name val : req)
optionalLenientQueryParam = queryParamHandler $ arr (absurd . snd)
