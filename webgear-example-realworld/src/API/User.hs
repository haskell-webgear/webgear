module API.User (
  create,
  login,
  current,
  update,
) where

import API.Common
import Control.Exception.Safe (try)
import qualified Crypto.JWT as JWT
import qualified Database.SQLite.Simple as DB
import qualified Model.User as Model
import qualified Network.HTTP.Types as HTTP
import WebGear.Server

type CreateUserRequest = Wrapped "user" Model.CreateUserPayload
type UserResponse = Wrapped "user" Model.UserRecord

create ::
  ( StdHandler h App
  , Get h (JSONBody CreateUserRequest)
  , Sets h (JSONBodyOrError UserResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
create jwk =
  withDoc "Create new user" "Create a new user in the store" $
    jsonRequestBody @CreateUserRequest badRequestBody $
      proc request -> do
        result <- createInDB -< request
        case result of
          Left e -> handleDBError -< e
          Right user ->
            setDescription okDescription <<< respondJsonA HTTP.ok200 -< Wrapped user :: UserResponse
  where
    createInDB = arrM $ \request -> do
      let userPayload = pick @(JSONBody CreateUserRequest) $ from request
      try $ runDBAction $ Model.create jwk (unwrap userPayload)

handleDBError ::
  (StdHandler h App, Set h (JSONBody ErrorResponse)) =>
  h DB.SQLError Response
handleDBError = proc e ->
  if DB.sqlError e == DB.ErrorConstraint
    then
      setDescription (dupDescription "user account") <<< respondJsonA HTTP.badRequest400 -< "Another user account exists with these values" :: ErrorResponse
    else respondJsonA HTTP.internalServerError500 -< showError e

--------------------------------------------------------------------------------

type LoginUserRequest = Wrapped "user" Model.LoginUserPayload

login ::
  ( StdHandler h App
  , Get h (JSONBody LoginUserRequest)
  , Sets h (JSONBodyOrError UserResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
login jwk =
  withDoc "Authenticate a user" "Authenticate a user and return their record" $
    jsonRequestBody @LoginUserRequest badRequestBody $
      proc request -> do
        result <- checkCreds -< request
        case result of
          Nothing ->
            setDescription resp403Description <<< respondJsonA HTTP.forbidden403 -< "Invalid credentials" :: ErrorResponse
          Just user ->
            setDescription okDescription <<< respondJsonA HTTP.ok200 -< Wrapped user :: UserResponse
  where
    checkCreds = arrM $ \request -> do
      let loginPayload = pick @(JSONBody LoginUserRequest) $ from request
      runDBAction $ Model.checkCredentials jwk (unwrap loginPayload)

--------------------------------------------------------------------------------

current ::
  ( StdHandler h App
  , Gets h [AuthHeader, RequiredAuth]
  , Sets h (JSONBodyOrError UserResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
current jwk =
  withDoc "Get current user" "Returns the record of authenticated user" $
    requiredTokenAuth jwk $
      proc request -> do
        result <- getAuthUser -< request
        case result of
          Nothing -> unwitnessA <<< setDescription (resp404Description "User") <<< notFound404 -< ()
          Just user ->
            setDescription okDescription <<< respondJsonA HTTP.ok200 -< Wrapped user :: UserResponse
  where
    getAuthUser = arrM $ \request -> do
      let userId = pick @RequiredAuth $ from request
      runDBAction $ Model.getByKey jwk userId

--------------------------------------------------------------------------------

type UpdateUserRequest = Wrapped "user" Model.UpdateUserPayload

update ::
  ( StdHandler h App
  , Gets h [AuthHeader, RequiredAuth, JSONBody UpdateUserRequest]
  , Sets h (JSONBodyOrError UserResponse)
  ) =>
  JWT.JWK ->
  RequestHandler h ts
update jwk =
  withDoc "Update current user" "Update the authenticated user" $
    requiredTokenAuth jwk $
      jsonRequestBody @UpdateUserRequest badRequestBody $
        proc request -> do
          result <- updateUser -< request
          case result of
            Left e -> handleDBError -< e
            Right Nothing -> unwitnessA <<< setDescription (resp404Description "User") <<< notFound404 -< ()
            Right (Just user) ->
              setDescription okDescription <<< respondJsonA HTTP.ok200 -< Wrapped user :: UserResponse
  where
    updateUser = arrM $ \request -> do
      let userId = pick @RequiredAuth $ from request
          userPayload = pick @(JSONBody UpdateUserRequest) $ from request
      try $ runDBAction $ Model.update jwk userId (unwrap userPayload)
