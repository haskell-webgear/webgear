{- | Traits and middleware to handle request query parameters.

 The `queryParam` middleware can extract a query parameter value trait
 and invoke another handler. An error handler is invoked if the parameter
 is missing or the parsing fails.

 The `optionalQueryParam` middleware is similar but will not invoke
 the error handling in case the prameter is missing. Instead, the
 trait value will be set to `Nothing` in that case.

 The `lenientQueryParam` middleware requires the parameter to be
 present. But the trait attribute will be set to 'Left' @msg@ if an
 error occurs while parsing it to a Haskell value. Here @msg@ will
 indicate the error in parsing.

 Finally, we have `optionalLenientQueryParam` which combines the
 behaviors of `optionalQueryParam` and `lenientQueryParam`. In this
 case, the parameter extraction never fails. Missing parameters and
 parse errors are indicated in the trait attribute passed to next
 handler.
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
import WebGear.Core.Trait (Absence, Attribute, Get, Prerequisite, With, probe)

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

type instance Attribute (QueryParam Required Strict name val) Request = val
type instance Absence (QueryParam Required Strict name val) = Either ParamNotFound ParamParseError

type instance Attribute (QueryParam Optional Strict name val) Request = Maybe val
type instance Absence (QueryParam Optional Strict name val) = ParamParseError

type instance Attribute (QueryParam Required Lenient name val) Request = Either Text val
type instance Absence (QueryParam Required Lenient name val) = ParamNotFound

type instance Attribute (QueryParam Optional Lenient name val) Request = Maybe (Either Text val)
type instance Absence (QueryParam Optional Lenient name val) = Void

type instance Prerequisite (QueryParam e p name val) ts = ()

queryParamHandler ::
  forall name val e p h ts.
  (Get h (QueryParam e p name val), ArrowChoice h) =>
  h (Request `With` ts, Absence (QueryParam e p name val)) Response ->
  Middleware h ts (QueryParam e p name val : ts)
queryParamHandler errorHandler nextHandler = proc request -> do
  result <- probe QueryParam -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right val -> nextHandler -< val
{-# INLINE queryParamHandler #-}

{- | Extract a query parameter and convert it to a value of type
 @val@.

 The associated trait attribute has type @val@.

 Example usage:

 > queryParam @"limit" @Integer errorHandler okHandler
-}
queryParam ::
  forall name val h ts.
  (Get h (QueryParam Required Strict name val), ArrowChoice h) =>
  h (Request `With` ts, Either ParamNotFound ParamParseError) Response ->
  Middleware h ts (QueryParam Required Strict name val : ts)
queryParam = queryParamHandler
{-# INLINE queryParam #-}

{- | Extract an optional query parameter and convert it to a value of
 type @val@.

 The associated trait attribute has type @Maybe val@; a @Nothing@
 value indicates that the parameter is missing from the request.

 Example usage:

 > optionalQueryParam @"limit" @Integer errorHandler okHandler
-}
optionalQueryParam ::
  forall name val h ts.
  (Get h (QueryParam Optional Strict name val), ArrowChoice h) =>
  h (Request `With` ts, ParamParseError) Response ->
  Middleware h ts (QueryParam Optional Strict name val : ts)
optionalQueryParam = queryParamHandler
{-# INLINE optionalQueryParam #-}

{- | Extract a query parameter and convert it to a value of type @val@.

 The associated trait attribute has type @Either Text val@. The
 parsing is done leniently and any errors are reported in the trait
 attribute.

 Example usage:

 > lenientQueryParam @"limit" @Integer errorHandler okHandler
-}
lenientQueryParam ::
  forall name val h ts.
  (Get h (QueryParam Required Lenient name val), ArrowChoice h) =>
  h (Request `With` ts, ParamNotFound) Response ->
  Middleware h ts (QueryParam Required Lenient name val : ts)
lenientQueryParam = queryParamHandler
{-# INLINE lenientQueryParam #-}

{- | Extract a query parameter and convert it to a value of type @val@.

 Example usage:

 > optionalLenientQueryParam @"Content-Length" @Integer handler

 The associated trait attribute has type @Maybe (Either Text
 val)@. The parsing is done leniently. Any parsing errors and
 missing query parameters are reported in the trait attribute.
-}
optionalLenientQueryParam ::
  forall name val h ts.
  (Get h (QueryParam Optional Lenient name val), ArrowChoice h) =>
  Middleware h ts (QueryParam Optional Lenient name val : ts)
optionalLenientQueryParam = queryParamHandler $ arr (absurd . snd)
{-# INLINE optionalLenientQueryParam #-}
