module API.User (
  create,
  login,
  current,
  update,
) where

import API.Common
import Control.Category ((.))
import Control.Exception.Safe (try)
import qualified Crypto.JWT as JWT
import qualified Database.Sqlite as DB
import qualified Model.User as Model
import qualified Network.HTTP.Types as HTTP
import Relude hiding ((.))
import WebGear.Server

type CreateUserRequest = Wrapped "user" Model.CreateUserPayload
type UserResponse = Wrapped "user" Model.UserRecord

create ::
  ( StdHandler h App
  , Get h (JSONBody CreateUserRequest) Request
  , Sets h (JSONBodyOrError UserResponse) Response
  ) =>
  JWT.JWK ->
  RequestHandler h ts
create jwk =
  withDoc "Create new user" "Create a new user in the store"
    $ jsonRequestBody @CreateUserRequest badRequestBody
    $ proc request -> do
      result <- createInDB -< request
      case result of
        Left e -> handleDBError -< e
        Right user ->
          respondJsonA HTTP.ok200
            . setDescription okDescription
            -<
              Wrapped user :: UserResponse
  where
    createInDB = arrM $ \request -> do
      let userPayload = pick @(JSONBody CreateUserRequest) $ from request
      try $ runDBAction $ Model.create jwk (unwrap userPayload)

handleDBError ::
  ( StdHandler h App
  , Sets h [RequiredResponseHeader "Content-Type" Text, JSONBody ErrorResponse] Response
  ) =>
  h DB.SqliteException Response
handleDBError = proc e ->
  if DB.seError e == DB.ErrorConstraint
    then
      respondJsonA HTTP.badRequest400
        . setDescription (dupDescription "user account")
        -<
          "Another user account exists with these values" :: ErrorResponse
    else respondJsonA HTTP.internalServerError500 -< show @ErrorResponse e

--------------------------------------------------------------------------------

type LoginUserRequest = Wrapped "user" Model.LoginUserPayload

login ::
  ( StdHandler h App
  , Get h (JSONBody LoginUserRequest) Request
  , Sets h (JSONBodyOrError UserResponse) Response
  ) =>
  JWT.JWK ->
  RequestHandler h ts
login jwk =
  withDoc "Authenticate a user" "Authenticate a user and return their record"
    $ jsonRequestBody @LoginUserRequest badRequestBody
    $ proc request -> do
      result <- checkCreds -< request
      case result of
        Nothing ->
          respondJsonA HTTP.forbidden403
            . setDescription resp403Description
            -<
              "Invalid credentials" :: ErrorResponse
        Just user ->
          respondJsonA HTTP.ok200
            . setDescription okDescription
            -<
              Wrapped user :: UserResponse
  where
    checkCreds = arrM $ \request -> do
      let loginPayload = pick @(JSONBody LoginUserRequest) $ from request
      runDBAction $ Model.checkCredentials jwk (unwrap loginPayload)

--------------------------------------------------------------------------------

current ::
  ( StdHandler h App
  , Get h RequiredAuth Request
  , Sets h (JSONBodyOrError UserResponse) Response
  ) =>
  JWT.JWK ->
  RequestHandler h ts
current jwk =
  withDoc "Get current user" "Returns the record of authenticated user"
    $ requiredTokenAuth jwk
    $ proc request -> do
      result <- getAuthUser -< request
      case result of
        Nothing -> unwitnessA . setDescription (resp404Description "User") . notFound404 -< ()
        Just user ->
          respondJsonA HTTP.ok200
            . setDescription okDescription
            -<
              Wrapped user :: UserResponse
  where
    getAuthUser = arrM $ \request -> do
      let userId = pick @RequiredAuth $ from request
      runDBAction $ Model.getByKey jwk userId

--------------------------------------------------------------------------------

type UpdateUserRequest = Wrapped "user" Model.UpdateUserPayload

update ::
  ( StdHandler h App
  , Gets h [RequiredAuth, JSONBody UpdateUserRequest] Request
  , Sets h (JSONBodyOrError UserResponse) Response
  ) =>
  JWT.JWK ->
  RequestHandler h ts
update jwk =
  withDoc "Update current user" "Update the authenticated user"
    $ requiredTokenAuth jwk
    $ jsonRequestBody @UpdateUserRequest badRequestBody
    $ proc request -> do
      result <- updateUser -< request
      case result of
        Left e -> handleDBError -< e
        Right Nothing -> unwitnessA . setDescription (resp404Description "User") . notFound404 -< ()
        Right (Just user) ->
          respondJsonA HTTP.ok200
            . setDescription okDescription
            -<
              Wrapped user :: UserResponse
  where
    updateUser = arrM $ \request -> do
      let userId = pick @RequiredAuth $ from request
          userPayload = pick @(JSONBody UpdateUserRequest) $ from request
      try $ runDBAction $ Model.update jwk userId (unwrap userPayload)
