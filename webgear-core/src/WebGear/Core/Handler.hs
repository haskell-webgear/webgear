{- |
 WebGear handlers
-}
module WebGear.Core.Handler (
  Handler (..),
  RoutePath (..),
  RouteMismatch (..),
  Description (..),
  Summary (..),
  RequestHandler,
  Middleware,
  routeMismatch,
  unwitnessA,
  (>->),
  (<-<),
) where

import Control.Arrow (Arrow, ArrowChoice, ArrowPlus, arr)
import Control.Arrow.Operations (ArrowError (..))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Exts (IsList (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response (..))
import WebGear.Core.Trait (With (unwitness))

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
 * Provide contextual documentation elements - description and summary
-}
class (ArrowChoice h, ArrowPlus h, ArrowError RouteMismatch h, Monad m) => Handler h m | h -> m where
  -- | Lift a monadic function to a handler arrow
  arrM :: (a -> m b) -> h a b

  -- | Consume all remaining path components with an arrow
  consumeRoute :: h RoutePath a -> h () a

  -- | Set a description of a part of an API
  setDescription :: Description -> h a a

  -- | Set a summary of a part of an API
  setSummary :: Summary -> h a a

-- | A handler arrow from a witnessed request to response.
type RequestHandler h req = h (Request `With` req) Response

-- | A middleware enhances a `RequestHandler` and produces another handler.
type Middleware h reqOut reqIn = RequestHandler h reqIn -> RequestHandler h reqOut

-- | Description associated with part of an API
newtype Description = Description {getDescription :: Text}
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (IsString)

-- | A summary associated with part of an API
newtype Summary = Summary {getSummary :: Text}
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (IsString)

-- | Indicates that a handler cannot process this route
data RouteMismatch = RouteMismatch
  deriving stock (Show, Eq, Ord)

instance Semigroup RouteMismatch where
  RouteMismatch <> RouteMismatch = RouteMismatch

instance Monoid RouteMismatch where
  mempty = RouteMismatch

-- | Indicates that the request does not match the current handler.
routeMismatch :: (ArrowError RouteMismatch h) => h a b
routeMismatch = proc _a -> raise -< RouteMismatch
{-# INLINE routeMismatch #-}

-- | Lifts `unwitness` into a handler arrow.
unwitnessA :: (Handler h m) => h (Response `With` ts) Response
unwitnessA = arr unwitness
{-# INLINE unwitnessA #-}

infixr 1 >->, <-<

{- | Thread a response through commands from left to right.

 For example, an HTTP 200 response with a body and Content-Type header
 can be generated with:

@
 (ok200 -< ())
   >-> (\resp -> setBody "text/plain" -< (resp, "Hello World"))
   >-> (\resp -> unwitnessA -< resp)
@
-}
(>->) :: (Arrow h) => h (env, stack) a -> h (env, (a, stack)) b -> h (env, stack) b
f >-> g = proc (env, stack) -> do
  a <- f -< (env, stack)
  g -< (env, (a, stack))
{-# INLINE (>->) #-}

{- | Thread a response through commands from right to left.

 For example, an HTTP 200 response with a body and Content-Type header
 can be generated with:

@
 (\resp -> unwitnessA -< resp)
   <-< (\resp -> setBody "text/plain" -< (resp, "Hello World"))
   <-< (ok200 -< ())
@
-}
(<-<) :: (Arrow h) => h (env, (a, stack)) b -> h (env, stack) a -> h (env, stack) b
f <-< g = proc (env, stack) -> do
  a <- g -< (env, stack)
  f -< (env, (a, stack))
{-# INLINE (<-<) #-}
