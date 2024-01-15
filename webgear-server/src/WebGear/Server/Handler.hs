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

import Control.Arrow (
  Arrow (..),
  ArrowChoice (..),
  ArrowPlus (..),
  ArrowZero (..),
  Kleisli (..),
 )
import Control.Arrow.Operations (ArrowError (..))
import qualified Control.Category as Cat
import Control.Monad.Except (
  ExceptT (..),
  MonadError (..),
  mapExceptT,
  runExceptT,
 )
import Control.Monad.State.Strict (
  MonadState (..),
  StateT (..),
  evalStateT,
  mapStateT,
 )
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.String (fromString)
import Data.Version (showVersion)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Paths_webgear_server (version)
import WebGear.Core.Handler (
  Description,
  Handler (..),
  RouteMismatch (..),
  RoutePath (..),
  Summary,
 )
import WebGear.Core.Request (Request (..))
import WebGear.Core.Response (Response (..), ResponseBody (..))
import WebGear.Core.Trait (With, wzero)

{- | An arrow implementing a WebGear server.

 A good first approximation is to consider ServerHandler to be
 equivalent to the function arrow @a -> m b@ where @m@ is a monad. It
 also supports routing and possibly failing the computation when the
 route does not match.
-}
newtype ServerHandler m a b = ServerHandler
  { unServerHandler :: a -> StateT RoutePath (ExceptT RouteMismatch m) b
  }
  deriving
    ( Cat.Category
    , Arrow
    , ArrowZero
    , ArrowPlus
    , ArrowChoice
    )
    via Kleisli (StateT RoutePath (ExceptT RouteMismatch m))

instance (Monad m) => ArrowError RouteMismatch (ServerHandler m) where
  {-# INLINE raise #-}
  raise :: ServerHandler m RouteMismatch b
  raise = ServerHandler throwError

  {-# INLINE handle #-}
  handle ::
    ServerHandler m a b ->
    ServerHandler m (a, RouteMismatch) b ->
    ServerHandler m a b
  (ServerHandler action) `handle` (ServerHandler errHandler) =
    ServerHandler $ \a ->
      action a `catchError` \e -> errHandler (a, e)

  {-# INLINE tryInUnless #-}
  tryInUnless ::
    ServerHandler m a b ->
    ServerHandler m (a, b) c ->
    ServerHandler m (a, RouteMismatch) c ->
    ServerHandler m a c
  tryInUnless (ServerHandler action) (ServerHandler resHandler) (ServerHandler errHandler) =
    ServerHandler $ \a ->
      f a `catchError` \e -> errHandler (a, e)
    where
      f a = do
        b <- action a
        resHandler (a, b)

instance (Monad m) => Handler (ServerHandler m) m where
  {-# INLINE arrM #-}
  arrM :: (a -> m b) -> ServerHandler m a b
  arrM f = ServerHandler $ lift . lift . f

  {-# INLINE consumeRoute #-}
  consumeRoute :: ServerHandler m RoutePath a -> ServerHandler m () a
  consumeRoute (ServerHandler h) =
    ServerHandler $
      \() -> do
        a <- get >>= h
        put (RoutePath [])
        pure a

  {-# INLINE setDescription #-}
  setDescription :: Description -> ServerHandler m a a
  setDescription _ = Cat.id

  {-# INLINE setSummary #-}
  setSummary :: Summary -> ServerHandler m a a
  setSummary _ = Cat.id

-- | Run a ServerHandler to produce a result or a route mismatch error.
runServerHandler ::
  (Monad m) =>
  -- | The handler to run
  ServerHandler m a b ->
  -- | Path used for routing
  RoutePath ->
  -- | Input value to the arrow
  a ->
  -- | The result of the arrow
  m (Either RouteMismatch b)
runServerHandler (ServerHandler h) path a =
  runExceptT $ evalStateT (h a) path
{-# INLINE runServerHandler #-}

-- | Convert a ServerHandler to a WAI application
toApplication :: ServerHandler IO (Request `With` '[]) Response -> Wai.Application
toApplication h rqt cont =
  runServerHandler h path request
    >>= processResponse
      . addServerHeader
      . mkWebGearResponse
  where
    request :: Request `With` '[]
    request = wzero $ Request rqt

    path :: RoutePath
    path = RoutePath $ Wai.pathInfo rqt

    mkWebGearResponse :: Either RouteMismatch Response -> Response
    mkWebGearResponse = fromRight $ Response HTTP.notFound404 [] $ ResponseBodyBuilder mempty

    addServerHeader :: Response -> Response
    addServerHeader = \case
      Response status hdrs body -> Response status (foldr insertServerHeader [] hdrs) body
      resp -> resp

    insertServerHeader :: HTTP.Header -> HTTP.ResponseHeaders -> HTTP.ResponseHeaders
    insertServerHeader hdr@(name, _) hdrs
      | name == HTTP.hServer = (HTTP.hServer, webGearServerHeader) : hdrs
      | otherwise = hdr : hdrs

    webGearServerHeader :: ByteString
    webGearServerHeader = fromString $ "WebGear/" ++ showVersion version

    processResponse :: Response -> IO Wai.ResponseReceived
    processResponse resp =
      case resp of
        ResponseCont f -> f cont
        ResponseRaw raw fallback -> cont $ Wai.responseRaw raw fallback
        Response status hdrs body ->
          cont $
            case body of
              ResponseBodyFile fpath fpart -> Wai.responseFile status hdrs fpath fpart
              ResponseBodyBuilder b -> Wai.responseBuilder status hdrs b
              ResponseBodyStream sb -> Wai.responseStream status hdrs sb
{-# INLINE toApplication #-}

{- | Transform a `ServerHandler` running in one monad to another monad.

 This is useful in cases where the server is running in a custom
 monad but you would like to convert it to a WAI application using
 `toApplication`.

 Example usage with a ReaderT monad stack:

@
 `toApplication` (transform f server)
   where
     server :: `ServerHandler` (ReaderT r IO) (`Request` \``With`\` '[]) `Response`
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
  ServerHandler $ mapStateT (mapExceptT f) . g
{-# INLINE transform #-}
