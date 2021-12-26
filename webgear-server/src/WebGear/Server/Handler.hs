{- |
 Server implementation of WebGear handlers
-}
module WebGear.Server.Handler (
  ServerHandler (..),
  RoutePath (..),
  runServerHandler,
  toApplication,
  transform,
) where

import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowPlus (..), ArrowZero (..))
import Control.Arrow.Operations (ArrowError (..))
import qualified Control.Category as Cat
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import qualified Data.HashMap.Strict as HM
import Data.String (fromString)
import Data.Version (showVersion)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Paths_webgear_server (version)
import WebGear.Core.Handler (Description, Handler (..), RouteMismatch (..), RoutePath (..), Summary)
import WebGear.Core.Request (Request (..))
import WebGear.Core.Response (Response (..), toWaiResponse)
import WebGear.Core.Trait (Linked, linkzero)

{- | An arrow implementing a WebGear server.

 It can be thought of equivalent to the function arrow @a -> m b@
 where @m@ is a monad. It also supports routing and possibly failing
 the computation when the route does not match.
-}
newtype ServerHandler m a b = ServerHandler {unServerHandler :: (a, RoutePath) -> m (Either RouteMismatch b, RoutePath)}

instance Monad m => Cat.Category (ServerHandler m) where
  {-# INLINEABLE id #-}
  id = ServerHandler $ \(a, s) -> pure (Right a, s)

  {-# INLINEABLE (.) #-}
  ServerHandler f . ServerHandler g = ServerHandler $ \(a, s) ->
    g (a, s) >>= \case
      (Left e, s') -> pure (Left e, s')
      (Right b, s') -> f (b, s')

instance Monad m => Arrow (ServerHandler m) where
  arr f = ServerHandler (\(a, s) -> pure (Right (f a), s))

  {-# INLINEABLE first #-}
  first (ServerHandler f) = ServerHandler $ \((a, c), s) ->
    f (a, s) >>= \case
      (Left e, s') -> pure (Left e, s')
      (Right b, s') -> pure (Right (b, c), s')

  {-# INLINEABLE second #-}
  second (ServerHandler f) = ServerHandler $ \((c, a), s) ->
    f (a, s) >>= \case
      (Left e, s') -> pure (Left e, s')
      (Right b, s') -> pure (Right (c, b), s')

instance Monad m => ArrowZero (ServerHandler m) where
  {-# INLINEABLE zeroArrow #-}
  zeroArrow = ServerHandler (\(_a, s) -> pure (Left mempty, s))

instance Monad m => ArrowPlus (ServerHandler m) where
  {-# INLINEABLE (<+>) #-}
  ServerHandler f <+> ServerHandler g = ServerHandler $ \(a, s) ->
    f (a, s) >>= \case
      (Left _e, _s') -> g (a, s)
      (Right b, s') -> pure (Right b, s')

instance Monad m => ArrowChoice (ServerHandler m) where
  {-# INLINEABLE left #-}
  left (ServerHandler f) = ServerHandler $ \(bd, s) ->
    case bd of
      Right d -> pure (Right (Right d), s)
      Left b ->
        f (b, s) >>= \case
          (Left e, s') -> pure (Left e, s')
          (Right c, s') -> pure (Right (Left c), s')

  {-# INLINEABLE right #-}
  right (ServerHandler f) = ServerHandler $ \(db, s) ->
    case db of
      Left d -> pure (Right (Left d), s)
      Right b ->
        f (b, s) >>= \case
          (Left e, s') -> pure (Left e, s')
          (Right c, s') -> pure (Right (Right c), s')

instance Monad m => ArrowError RouteMismatch (ServerHandler m) where
  {-# INLINEABLE raise #-}
  raise = ServerHandler $ \(e, s) -> pure (Left e, s)

  {-# INLINEABLE handle #-}
  (ServerHandler action) `handle` (ServerHandler errHandler) = ServerHandler $ \(a, s) ->
    action (a, s) >>= \case
      (Left e, s') -> errHandler ((a, e), s')
      (Right b, s') -> pure (Right b, s')

  {-# INLINEABLE tryInUnless #-}
  tryInUnless (ServerHandler action) (ServerHandler resHandler) (ServerHandler errHandler) =
    ServerHandler $ \(a, s) ->
      action (a, s) >>= \case
        (Left e, s') -> errHandler ((a, e), s')
        (Right b, s') -> resHandler ((a, b), s')

instance Monad m => Handler (ServerHandler m) m where
  {-# INLINEABLE arrM #-}
  arrM :: (a -> m b) -> ServerHandler m a b
  arrM f = ServerHandler $ \(a, s) -> f a >>= \b -> pure (Right b, s)

  {-# INLINEABLE consumeRoute #-}
  consumeRoute :: ServerHandler m RoutePath a -> ServerHandler m () a
  consumeRoute (ServerHandler h) = ServerHandler $
    \((), path) -> h (path, RoutePath [])

  {-# INLINEABLE setDescription #-}
  setDescription :: Description -> ServerHandler m a a
  setDescription _ = Cat.id

  {-# INLINEABLE setSummary #-}
  setSummary :: Summary -> ServerHandler m a a
  setSummary _ = Cat.id

-- | Run a ServerHandler to produce a result or a route mismatch error.
runServerHandler ::
  Monad m =>
  -- | The handler to run
  ServerHandler m a b ->
  -- | Path used for routing
  RoutePath ->
  -- | Input value to the arrow
  a ->
  -- | The result of the arrow
  m (Either RouteMismatch b)
runServerHandler (ServerHandler h) path a = fst <$> h (a, path)

-- | Convert a ServerHandler to a WAI application
toApplication :: ServerHandler IO (Linked '[] Request) Response -> Wai.Application
toApplication h rqt cont =
  runServerHandler h path request
    >>= cont . toWaiResponse . addServerHeader . mkWebGearResponse
  where
    request :: Linked '[] Request
    request = linkzero $ Request rqt

    path :: RoutePath
    path = RoutePath $ Wai.pathInfo rqt

    mkWebGearResponse :: Either RouteMismatch Response -> Response
    mkWebGearResponse = fromRight (Response HTTP.notFound404 [] mempty)

    addServerHeader :: Response -> Response
    addServerHeader resp@Response{..} = resp{responseHeaders = responseHeaders <> webGearServerHeader}

{- | Transform a `ServerHandler` running in one monad to another monad.

 This is useful in cases where the server is running in a custom
 monad but you would like to convert it to a WAI application using
 `toApplication`.

 Example usage with a ReaderT monad stack:

@
 `toApplication` (transform f server)
   where
     server :: `ServerHandler` (ReaderT r IO) (`Linked` '[] `Request`) `Response`
     server = ....

     f :: ReaderT r IO a -> IO a
     f action = runReaderT action r
@
-}
transform ::
  (forall x. m x -> n x) ->
  ServerHandler m a b ->
  ServerHandler n a b
transform f (ServerHandler g) =
  ServerHandler $ f . g

webGearServerHeader :: HM.HashMap HTTP.HeaderName ByteString
webGearServerHeader = HM.singleton HTTP.hServer (fromString $ "WebGear/" ++ showVersion version)
