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

newtype RoutePath = RoutePath [Text]
  deriving newtype (Show, Eq)

instance IsList RoutePath where
  type Item RoutePath = Text
  fromList = RoutePath
  toList (RoutePath ps) = ps

-- | A handler is an arrow with a monadic context.
class (ArrowChoice h, ArrowPlus h, ArrowError RouteMismatch h, Monad m) => Handler h m | h -> m where
  -- | Lift a monadic function to a handler arrow
  arrM :: (a -> m b) -> h a b

  -- | Consume all remaining path components with an arrow
  consumeRoute :: h RoutePath a -> h () a

type RequestHandler h req = h (Linked req Request) Response

type Middleware h reqOut reqIn = RequestHandler h reqIn -> RequestHandler h reqOut

-- | Indicates that a handler cannot process this route
data RouteMismatch = RouteMismatch
  deriving stock (Show, Eq, Ord)

instance Semigroup RouteMismatch where
  RouteMismatch <> RouteMismatch = RouteMismatch

instance Monoid RouteMismatch where
  mempty = RouteMismatch

routeMismatch :: ArrowError RouteMismatch h => h a b
routeMismatch = proc _a -> raise -< RouteMismatch

unlinkA :: Handler h m => h (Linked ts Response) Response
unlinkA = arr unlink
