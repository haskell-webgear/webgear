module WebGear (application) where

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
  ( StdHandler h AppM
  , Gets h [UserIdPathVar, Body JSON User] Request
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body JSON User] Response
  ) =>
  h (Request `With` '[]) Response
allRoutes =
  -- Non-TH version:
  --   path "/v1/users" $ pathVar @"userId" @Int $ pathEnd
  [route| /v1/users/userId:Int |]
    $ method GET getUser
    <+> method PUT putUser
    <+> method DELETE deleteUser

getUser ::
  forall h req.
  ( HasTrait UserIdPathVar req
  , StdHandler h AppM
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body JSON User] Response
  ) =>
  h (Request `With` req) Response
getUser = findUser >>> respond
  where
    findUser :: h (Request `With` req) (Maybe User)
    findUser = arrM $ \request -> do
      let uid = pick @UserIdPathVar $ from request
      store <- ask
      lookupUser store (UserId uid)

    respond :: h (Maybe User) Response
    respond = proc maybeUser -> case maybeUser of
      Nothing -> unwitnessA <<< notFound404 -< ()
      Just u -> respondA HTTP.ok200 JSON -< u

putUser ::
  forall h req.
  ( HasTrait UserIdPathVar req
  , StdHandler h AppM
  , Get h (Body JSON User) Request
  , Sets h [RequiredResponseHeader "Content-Type" Text, Body JSON User] Response
  ) =>
  h (Request `With` req) Response
putUser = requestBody @User JSON badPayload $ doUpdate >>> respond
  where
    badPayload :: h (Request `With` req, Text) Response
    badPayload = proc _ ->
      respondA HTTP.badRequest400 PlainText -< "Invalid body payload" :: Text

    doUpdate :: (HaveTraits [UserIdPathVar, Body JSON User] ts) => h (Request `With` ts) User
    doUpdate = arrM $ \request -> do
      let uid = pick @UserIdPathVar $ from request
          user = pick @(Body JSON User) $ from request
          user' = user{userId = UserId uid}
      store <- ask
      addUser store user'
      pure user'

    respond :: h User Response
    respond = proc user ->
      respondA HTTP.ok200 JSON -< user

deleteUser ::
  forall h req.
  (HasTrait UserIdPathVar req, StdHandler h AppM) =>
  h (Request `With` req) Response
deleteUser = doDelete >>> respond
  where
    doDelete :: h (Request `With` req) Bool
    doDelete = arrM $ \request -> do
      let uid = pick @UserIdPathVar $ from request
      store <- ask
      removeUser store (UserId uid)

    respond :: h Bool Response
    respond = proc removed ->
      if removed
        then unwitnessA <<< noContent204 -< ()
        else unwitnessA <<< notFound404 -< ()

--------------------------------------------------------------------------------

-- | The application server

--------------------------------------------------------------------------------
application :: UserStore -> Application
application store = toApplication $ transform appToRouter allRoutes
  where
    appToRouter :: AppM a -> IO a
    appToRouter = flip runReaderT store
