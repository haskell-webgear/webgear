module WebGear where

import Control.Arrow
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Model
import Network.HTTP.Types (StdMethod (..))
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application)
import WebGear.Server

--------------------------------------------------------------------------------
-- Routes of the API
--------------------------------------------------------------------------------
-- The route handlers run in the App monad
type AppM = ReaderT UserStore IO

type UserIdPathVar = PathVar "userId" Int

allRoutes ::
  StdHandler
    h
    AppM
    [UserIdPathVar, JSONBody User]
    [Body Text, RequiredHeader "Content-Type" Text, JSONBody User] =>
  h (Linked '[] Request) Response
allRoutes =
  -- TH version: [route| /v1/users/userId:Int |]
  path "/v1/users" $
    pathVar @"userId" @Int $
      pathEnd $
        method GET getUser
          <+> method PUT putUser
          <+> method DELETE deleteUser

getUser ::
  forall h req.
  ( HasTrait UserIdPathVar req
  , StdHandler h AppM '[] [RequiredHeader "Content-Type" Text, JSONBody User]
  ) =>
  h (Linked req Request) Response
getUser = findUser >>> respond
  where
    findUser :: h (Linked req Request) (Maybe User)
    findUser = arrM $ \request -> do
      let uid = pick @UserIdPathVar $ from request
      store <- ask
      lookupUser store (UserId uid)

    respond :: h (Maybe User) Response
    respond = proc maybeUser -> case maybeUser of
      Nothing -> unlinkA <<< notFound404 -< ()
      Just u -> unlinkA <<< respondJsonA HTTP.ok200 -< u

putUser ::
  forall h req.
  ( HasTrait UserIdPathVar req
  , StdHandler
      h
      AppM
      '[JSONBody User]
      [RequiredHeader "Content-Type" Text, JSONBody User, Body Text]
  ) =>
  h (Linked req Request) Response
putUser = jsonRequestBody @User badPayload $ doUpdate >>> respond
  where
    badPayload :: h (Linked req Request, Text) Response
    badPayload = proc _ ->
      unlinkA <<< respondA HTTP.badRequest400 "text/plain" -< "Invalid body payload" :: Text

    doUpdate :: HaveTraits [UserIdPathVar, JSONBody User] ts => h (Linked ts Request) User
    doUpdate = arrM $ \request -> do
      let uid = pick @UserIdPathVar $ from request
          user = pick @(JSONBody User) $ from request
          user' = user{userId = UserId uid}
      store <- ask
      addUser store user'
      pure user'

    respond :: h User Response
    respond = proc user ->
      unlinkA <<< respondJsonA HTTP.ok200 -< user

deleteUser ::
  forall h req.
  (HasTrait UserIdPathVar req, StdHandler h AppM '[] '[]) =>
  h (Linked req Request) Response
deleteUser = doDelete >>> respond
  where
    doDelete :: h (Linked req Request) Bool
    doDelete = arrM $ \request -> do
      let uid = pick @UserIdPathVar $ from request
      store <- ask
      removeUser store (UserId uid)

    respond :: h Bool Response
    respond = proc removed ->
      if removed
        then unlinkA <<< noContent204 -< ()
        else unlinkA <<< notFound404 -< ()

--------------------------------------------------------------------------------

-- | The application server

--------------------------------------------------------------------------------
application :: UserStore -> Application
application store = toApplication $ transform appToRouter allRoutes
  where
    appToRouter :: AppM a -> IO a
    appToRouter = flip runReaderT store
