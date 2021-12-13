{- |
 Handle request/response body payloads
-}
module WebGear.Core.Middleware.Body (
  -- * Traits
  Body (..),
  JSONBody (..),

  -- * Middlewares
  requestBody,
  jsonRequestBody',
  jsonRequestBody,
  respondA,
  respondJsonA,
  respondJsonA',
  setBody,
  setBodyWithoutContentType,
  setJSONBody,
  setJSONBodyWithoutContentType,
  setJSONBody',
) where

import Control.Arrow (ArrowChoice, returnA, (<<<))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Media as HTTP
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Middleware)
import WebGear.Core.Middleware.Header (Header (..), RequiredHeader)
import WebGear.Core.Middleware.Status (Status, mkResponse)
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get, Linked, Set, Sets, Trait (..), TraitAbsence (..), plant, probe)

-- | Request or response body with a type @t@.
newtype Body (t :: Type) = Body (Maybe HTTP.MediaType)

instance Trait (Body t) Request where
  type Attribute (Body t) Request = t

instance TraitAbsence (Body t) Request where
  type Absence (Body t) Request = Text

instance Trait (Body t) Response where
  type Attribute (Body t) Response = t

-- | A 'Trait' for converting a JSON formatted body into a value.
newtype JSONBody (t :: Type) = JSONBody (Maybe HTTP.MediaType)

instance Trait (JSONBody t) Request where
  type Attribute (JSONBody t) Request = t

instance TraitAbsence (JSONBody t) Request where
  type Absence (JSONBody t) Request = Text

instance Trait (JSONBody t) Response where
  type Attribute (JSONBody t) Response = t

{- | Middleware to extract a request body.

 The @nextHandler@ is invoked after successfully extracting the body
 and the @errorHandler@ is invoked when there is an error.

 Usage:

 > requestBody @t (Just "text/plain") errorHandler nextHandler
-}
requestBody ::
  forall t h req.
  (Get h (Body t) Request, ArrowChoice h) =>
  -- | Optional media type of the body
  Maybe HTTP.MediaType ->
  -- | Error handler in case body cannot be retrieved
  h (Linked req Request, Text) Response ->
  Middleware h req (Body t : req)
requestBody mediaType errorHandler nextHandler = proc request -> do
  result <- probe (Body mediaType) -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right t -> nextHandler -< t

{- | Parse the request body as JSON and convert it to a value of type
   @t@.

 The @nextHandler@ is invoked when the body is parsed successfully and
 the @errorHandler@ is invoked when there is a parsing failure.

 Usage:

 > jsonRequestBody @t errorHandler nextHandler
-}
jsonRequestBody' ::
  forall t h req.
  (Get h (JSONBody t) Request, ArrowChoice h) =>
  -- | Optional media type of the body
  Maybe HTTP.MediaType ->
  -- | Error handler in case body cannot be retrieved
  h (Linked req Request, Text) Response ->
  Middleware h req (JSONBody t : req)
jsonRequestBody' mediaType errorHandler nextHandler = proc request -> do
  result <- probe (JSONBody mediaType) -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right t -> nextHandler -< t

-- | Same as 'jsonRequestBody'' but with a media type @application/json@.
jsonRequestBody ::
  forall t h req.
  (Get h (JSONBody t) Request, ArrowChoice h) =>
  -- | error handler
  h (Linked req Request, Text) Response ->
  Middleware h req (JSONBody t : req)
jsonRequestBody = jsonRequestBody' (Just "application/json")

{- | Set the response body along with a media type.

  The media type value is used to set the "Content-Type" header in the response.
-}
setBody ::
  forall body h ts.
  Sets h [Body body, RequiredHeader "Content-Type" Text] Response =>
  -- | The media type of the response body
  HTTP.MediaType ->
  h (Linked ts Response, body) (Linked (RequiredHeader "Content-Type" Text : Body body : ts) Response)
setBody mediaType = proc (r, body) -> do
  r' <- plant (Body (Just mediaType)) -< (r, body)
  let mt = decodeUtf8 $ HTTP.renderHeader mediaType
  returnA <<< plant Header -< (r', mt)

-- | Set the response body without specifying any media type.
setBodyWithoutContentType ::
  forall body h ts.
  Set h (Body body) Response =>
  h (Linked ts Response, body) (Linked (Body body : ts) Response)
setBodyWithoutContentType = plant (Body Nothing)

{- | Set the response body to a JSON value along with a media type.

  The media type value is used to set the "Content-Type" header in the response.
-}
setJSONBody' ::
  forall body h ts.
  Sets h [JSONBody body, RequiredHeader "Content-Type" Text] Response =>
  -- | The media type of the response body
  HTTP.MediaType ->
  h (Linked ts Response, body) (Linked (RequiredHeader "Content-Type" Text : JSONBody body : ts) Response)
setJSONBody' mediaType = proc (r, body) -> do
  r' <- plant (JSONBody (Just mediaType)) -< (r, body)
  let mt = decodeUtf8 $ HTTP.renderHeader mediaType
  returnA <<< plant Header -< (r', mt)

{- | Set the response body to a JSON value.

  The "Content-Type" header will be set to "application/json".
-}
setJSONBody ::
  forall body h ts.
  Sets h [JSONBody body, RequiredHeader "Content-Type" Text] Response =>
  h (Linked ts Response, body) (Linked (RequiredHeader "Content-Type" Text : JSONBody body : ts) Response)
setJSONBody = setJSONBody' "application/json"

{- | Set the response body to a JSON value without specifying any
 media type.
-}
setJSONBodyWithoutContentType ::
  forall body h ts.
  Set h (JSONBody body) Response =>
  h (Linked ts Response, body) (Linked (JSONBody body : ts) Response)
setJSONBodyWithoutContentType = plant (JSONBody Nothing)

{- | A convenience arrow to generate a response specifying a status and body.

 The "Content-Type" header will be set to the specified media type
 value.
-}
respondA ::
  forall body h.
  Sets h [Status, Body body, RequiredHeader "Content-Type" Text] Response =>
  -- | Response status
  HTTP.Status ->
  -- | Media type of the response body
  HTTP.MediaType ->
  h body (Linked [RequiredHeader "Content-Type" Text, Body body, Status] Response)
respondA status mediaType = proc body -> do
  r <- mkResponse status -< ()
  setBody mediaType -< (r, body)

{- | A convenience arrow to generate a response specifying a status and
   JSON body.

 The "Content-Type" header will be set to "application/json".
-}
respondJsonA ::
  forall body h.
  Sets h [Status, JSONBody body, RequiredHeader "Content-Type" Text] Response =>
  -- | Response status
  HTTP.Status ->
  h body (Linked [RequiredHeader "Content-Type" Text, JSONBody body, Status] Response)
respondJsonA status = respondJsonA' status "application/json"

{- | A convenience arrow to generate a response specifying a status and
   JSON body.

 The "Content-Type" header will be set to the specified media type
 value.
-}
respondJsonA' ::
  forall body h.
  Sets h [Status, JSONBody body, RequiredHeader "Content-Type" Text] Response =>
  -- | Response status
  HTTP.Status ->
  -- | Media type of the response body
  HTTP.MediaType ->
  h body (Linked [RequiredHeader "Content-Type" Text, JSONBody body, Status] Response)
respondJsonA' status mediaType = proc body -> do
  r <- mkResponse status -< ()
  setJSONBody' mediaType -< (r, body)
