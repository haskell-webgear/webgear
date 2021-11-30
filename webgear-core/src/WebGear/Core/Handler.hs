-- |
-- Copyright        : (c) Raghu Kaippully, 2020-2021
-- License          : MPL-2.0
-- Maintainer       : rkaippully@gmail.com
--
-- WebGear handlers
--
module WebGear.Core.Handler
  ( Handler (..)
  , RoutePath (..)
  , RouteMismatch(..)
  , Middleware
  , routeMismatch
  , respondA
  , (>#>)
  , (<#<)
  ) where

import Control.Arrow (ArrowChoice, ArrowPlus, arr)
import Control.Arrow.Operations (ArrowError (..))
import Data.Text (Text)
import GHC.Exts (IsList (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Linked (unlink))


newtype RoutePath = RoutePath [Text]
  deriving newtype (Show, Eq)

instance IsList RoutePath where
  type Item RoutePath = Text
  fromList = RoutePath
  toList (RoutePath ps) = ps

-- | A handler is an arrow with a monadic context.
class (ArrowChoice h, ArrowPlus h, ArrowError RouteMismatch h, Monad m) => Handler h m | h -> m where
  -- | Lift a monadic function to a handler arrow
  handler :: (a -> m b) -> h a b

  -- | Consume all remaining path components with an arrow
  consumeRoute :: h RoutePath a -> h () a

type RequestHandler h req = h (Linked req Request) Response

type Middleware h reqOut reqIn = RequestHandler h reqIn -> RequestHandler h reqOut

respondA :: Handler h m => h (Linked ts Response) Response
respondA = arr unlink

-- | Indicates that a handler cannot process this route
data RouteMismatch = RouteMismatch
  deriving stock (Show, Eq, Ord)

instance Semigroup RouteMismatch where
  RouteMismatch <> RouteMismatch = RouteMismatch

instance Monoid RouteMismatch where
  mempty = RouteMismatch

routeMismatch :: ArrowError RouteMismatch h => h a b
routeMismatch = proc _a -> raise -< RouteMismatch

infix 1 >#>, <#<

(>#>) :: ArrowChoice a
      => a (env, stack) response1
      -> a (env, (response1, stack)) response2
      -> a (env, stack) response2
arr1 >#> arr2 = proc (e, s) -> do
  r <- arr1 -< (e, s)
  arr2 -< (e, (r, s))

(<#<) :: ArrowChoice a
      => a (env, (response1, stack)) response2
      -> a (env, stack) response1
      -> a (env, stack) response2
arr1 <#< arr2 = proc (e, s) -> do
  r <- arr2 -< (e, s)
  arr1 -< (e, (r, s))
