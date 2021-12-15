{- |
 WebGear handlers
-}
module WebGear.Core.Handler (
  Handler (..),
  RoutePath (..),
  RouteMismatch (..),
  RequestHandler,
  Middleware,
  routeMismatch,
  unlinkA,
) where

import Control.Arrow (ArrowChoice, ArrowPlus, arr)
import Control.Arrow.Operations (ArrowError (..))
import Data.Text (Text)
import GHC.Exts (IsList (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (Linked (unlink))

-- | Parts of the request path used by the routing machinery
newtype RoutePath = RoutePath [Text]
  deriving newtype (Show, Eq)

instance IsList RoutePath where
  type Item RoutePath = Text
  fromList = RoutePath
  toList (RoutePath ps) = ps

{- | A handler is an arrow with a monadic context.

 Handlers have the following capabilities:

 * Lift a monadic action into a handler arrow.
 * Implement `ArrowChoice` typeclass so that conditionals can be used in arrow code.
 * Implement `ArrowPlus` for routing requests to specific handlers.
-}
class (ArrowChoice h, ArrowPlus h, ArrowError RouteMismatch h, Monad m) => Handler h m | h -> m where
  -- | Lift a monadic function to a handler arrow
  arrM :: (a -> m b) -> h a b

  -- | Consume all remaining path components with an arrow
  consumeRoute :: h RoutePath a -> h () a

-- | A handler arrow from a linked request to response.
type RequestHandler h req = h (Linked req Request) Response

-- | A middleware enhances a `RequestHandler` and produces another handler.
type Middleware h reqOut reqIn = RequestHandler h reqIn -> RequestHandler h reqOut

-- | Indicates that a handler cannot process this route
data RouteMismatch = RouteMismatch
  deriving stock (Show, Eq, Ord)

instance Semigroup RouteMismatch where
  RouteMismatch <> RouteMismatch = RouteMismatch

instance Monoid RouteMismatch where
  mempty = RouteMismatch

-- | Indicates that the request does not match the current handler.
routeMismatch :: ArrowError RouteMismatch h => h a b
routeMismatch = proc _a -> raise -< RouteMismatch

-- | Lifts `unlink` into a handler arrow.
unlinkA :: Handler h m => h (Linked ts Response) Response
unlinkA = arr unlink
