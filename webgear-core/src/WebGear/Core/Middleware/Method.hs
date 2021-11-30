-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- Handle to HTTP methods.
--
module WebGear.Core.Middleware.Method
  ( Method (..)
  , MethodMismatch (..)
  , method
  ) where

import Control.Arrow (ArrowChoice (..), (>>>))
import Control.Arrow.Operations (ArrowError)
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Middleware, RouteMismatch, routeMismatch)
import WebGear.Core.Request (Request)
import WebGear.Core.Trait (Get (..), Trait (..), TraitAbsence (..), probe)


-- | A 'Trait' for capturing the HTTP method of a request
newtype Method = Method HTTP.StdMethod

-- | Failure to match method against an expected value
data MethodMismatch = MethodMismatch
  { expectedMethod :: HTTP.Method
  , actualMethod   :: HTTP.Method
  }

instance Trait Method Request where
  type Attribute Method Request = HTTP.StdMethod

instance TraitAbsence Method Request where
  type Absence Method Request = MethodMismatch

-- | Check whether the request has a specified HTTP method.
--
-- Example usage:
--
-- > method @GET handler
--
-- It is also idiomatic to use the template haskell quasiquoter
-- 'WebGear.Core.Middleware.Path.match' in cases where both HTTP method
-- and path needs to be matched.
--
method :: (Get h Method Request, ArrowChoice h, ArrowError RouteMismatch h)
       => HTTP.StdMethod
       -> Middleware h req (Method : req)
method m nextHandler = probe (Method m) >>> routeMismatch ||| nextHandler
