{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative (Alternative (..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Web.HttpApiData (FromHttpApiData)
import WebGear.Server

--------------------------------------------------------------------------------
-- An example program that uses WebGear to build a simple HTTP API to
-- perform CRUD operations on user records.
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Model for users
--------------------------------------------------------------------------------
data User = User
  { userId :: UserId
  , userName :: Text
  , dateOfBirth :: Day
  , gender :: Gender
  , emailAddress :: Text
  }
  deriving (Generic, FromJSON, ToJSON)

newtype UserId = UserId Int
  deriving (Eq, FromJSON, ToJSON, Hashable, FromHttpApiData) via Int

data Gender = Male | Female | OtherGender
  deriving (Generic, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- An in-memory store and associated operations for users
--------------------------------------------------------------------------------
newtype UserStore = UserStore (IORef (HM.HashMap UserId User))

addUser :: (MonadIO m) => UserStore -> User -> m ()
addUser (UserStore ref) user = liftIO $ modifyIORef ref (HM.insert (userId user) user)

lookupUser :: (MonadIO m) => UserStore -> UserId -> m (Maybe User)
lookupUser (UserStore ref) uid = liftIO (HM.lookup uid <$> readIORef ref)

removeUser :: (MonadIO m) => UserStore -> UserId -> m Bool
removeUser store@(UserStore ref) uid = liftIO $ do
  u <- lookupUser store uid
  modifyIORef ref (HM.delete uid)
  pure $ isJust u

--------------------------------------------------------------------------------
-- Routes of the API
--------------------------------------------------------------------------------
type IntUserId = PathVar "userId" UserId
type Auth = BasicAuth App () Credentials

authConfig :: BasicAuth App () Credentials
authConfig =
  BasicAuth'
    { toBasicAttribute = \creds ->
        pure $
          if creds == Credentials "panther" "forever"
            then Right creds
            else Left ()
    }

-- The route handlers run in the App monad
newtype App a = App {unApp :: ReaderT UserStore IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Alternative
    , Monad
    , MonadIO
    , MonadReader UserStore
    )

userRoutes :: RequestHandler (ServerHandler App) ts
userRoutes =
  -- non-TH version: path @"/v1/users" . pathVar @"userId" @Int
  [match| /v1/users/userId:UserId |]
    (publicRoutes <+> protectedRoutes)

-- | Routes accessible without any authentication
publicRoutes ::
  ( HasTrait IntUserId ts
  , StdHandler h App
  , Set h (Body JSON User)
  ) =>
  RequestHandler h ts
publicRoutes = getUser

-- | Routes that require HTTP basic authentication
protectedRoutes ::
  forall h ts.
  ( HasTrait IntUserId ts
  , StdHandler h App
  , Gets h '[AuthorizationHeader "Basic", BasicAuth App () Credentials, Body JSON User]
  , Set h (Body JSON User)
  ) =>
  RequestHandler h ts
protectedRoutes =
  optionalLenientHeader @"Authorization" @(AuthToken "Basic") $
    basicAuth authConfig authError $
      putUser
        <+> deleteUser
  where
    authError :: h (Request `With` xs, BasicAuthError ()) Response
    authError = proc (_request, err) -> case err of
      BasicAuthAttributeError () ->
        respondA HTTP.forbidden403 PlainText -< "Forbidden" :: Text
      _ ->
        respondA HTTP.unauthorized401 PlainText -< "Unauthorized" :: Text

getUser ::
  forall h ts.
  ( HasTrait IntUserId ts
  , StdHandler h App
  , Set h (Body JSON User)
  ) =>
  RequestHandler h ts
getUser = method HTTP.GET $
  proc request -> do
    let uid = pick @IntUserId $ from request
    maybeUser <- fetchUser -< uid
    case maybeUser of
      Nothing -> unwitnessA <<< notFound404 -< ()
      Just user -> respondA HTTP.ok200 JSON -< user
  where
    fetchUser :: h UserId (Maybe User)
    fetchUser = arrM $ \uid -> do
      store <- ask
      lookupUser store uid

putUser ::
  forall h ts.
  ( HaveTraits [Auth, IntUserId] ts
  , StdHandler h App
  , Get h (Body JSON User)
  , Set h (Body JSON User)
  ) =>
  RequestHandler h ts
putUser = method HTTP.PUT $
  requestBody @User JSON badRequestBody $
    proc request -> do
      let uid = pick @IntUserId $ from request
          user = pick @(Body JSON User) $ from request
          user' = user{userId = uid}
      doAdd -< (request, user')
      respondA HTTP.ok200 JSON -< user'
  where
    badRequestBody :: h a Response
    badRequestBody = proc _ ->
      respondA HTTP.badRequest400 PlainText -< "Could not parse body" :: Text

    doAdd = arrM $ \(request, user) -> do
      store <- ask
      addUser store user
      logActivity request "updated"

deleteUser ::
  forall h ts.
  ( HaveTraits [Auth, IntUserId] ts
  , StdHandler h App
  ) =>
  RequestHandler h ts
deleteUser = method HTTP.DELETE $
  proc request -> do
    let uid = pick @IntUserId $ from request
    removed <- doRemove -< (request, uid)
    if removed
      then unwitnessA <<< noContent204 -< ()
      else unwitnessA <<< notFound404 -< ()
  where
    doRemove = arrM $ \(request, uid) -> do
      store <- ask
      found <- removeUser store uid
      when found $
        logActivity request "deleted"
      pure found

logActivity :: (MonadIO m, HasTrait Auth ts) => Request `With` ts -> String -> m ()
logActivity request msg = do
  let name = credentialsUsername $ pick @Auth $ from request
  liftIO $ putStrLn $ msg <> ": by " <> show name

--------------------------------------------------------------------------------
-- The application server
-------------------------------------------------------------------------------
application :: UserStore -> Application
application store = toApplication $ transform appToRouter userRoutes
  where
    appToRouter :: App a -> IO a
    appToRouter = flip runReaderT store . unApp

main :: IO ()
main = do
  store <- newIORef HM.empty
  let settings =
        Warp.defaultSettings
          & Warp.setHost "127.0.0.1"
          & Warp.setPort 3000
  Warp.runSettings settings (application $ UserStore store)
