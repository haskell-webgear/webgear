-- | Traits and middlewares to handle HTTP methods.
module WebGear.Core.Trait.Method (
  Method (..),
  MethodMismatch (..),
  method,
) where

import Control.Arrow (ArrowChoice (..), (>>>))
import Control.Arrow.Operations (ArrowError)
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Middleware, RouteMismatch, routeMismatch)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Absence, Attribute, Get (..), Prerequisite, probe)

-- | A trait for capturing the HTTP method of a request
newtype Method = Method HTTP.StdMethod

-- | Failure to match method against an expected value
data MethodMismatch = MethodMismatch
  { expectedMethod :: HTTP.Method
  , actualMethod :: HTTP.Method
  }

type instance Attribute Method Request = HTTP.StdMethod
type instance Absence Method = MethodMismatch
type instance Prerequisite Method ts = ()

{- | Check whether the request has a specified HTTP method.

 Example usage:

 > method @GET handler

 If the request does not have the specified method, another handler
 will be tried.

 It is also idiomatic to use the template haskell quasiquoter
 'WebGear.Core.Trait.Path.match' or 'WebGear.Core.Trait.Path.route' in
 cases where both an HTTP method and a path need to be matched.
-}
method ::
  (Get h Method, ArrowChoice h, ArrowError RouteMismatch h) =>
  HTTP.StdMethod ->
  Middleware h ts (Method : ts)
method m nextHandler = probe (Method m) >>> routeMismatch ||| nextHandler
{-# INLINE method #-}
