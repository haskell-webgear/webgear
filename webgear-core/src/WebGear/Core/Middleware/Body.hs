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

-- | Body with a type @body@ that can be converted to a 'ByteString'.
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

requestBody ::
  (Get h (Body t) Request, ArrowChoice h) =>
  Maybe HTTP.MediaType ->
  -- | error handler
  h (Linked req Request, Text) Response ->
  Middleware h req (Body t : req)
requestBody mediaType errorHandler nextHandler = proc request -> do
  result <- probe (Body mediaType) -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right t -> nextHandler -< t

{- | Parse the request body as JSON and convert it to a value via a
 'FromJSON' instance.

 The @okHandler@ is invoked when the body is parsed successfully and
 the @errorHandler@ is invoked when there is a parsing failure.

 Usage for a type @t@ which has a 'FromJSON' instance:

 > jsonRequestBody @t errorHandler
-}
jsonRequestBody' ::
  forall t h req.
  (Get h (JSONBody t) Request, ArrowChoice h) =>
  Maybe HTTP.MediaType ->
  -- | error handler
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

setBody ::
  forall body h ts.
  Sets h [Body body, RequiredHeader "Content-Type" Text] Response =>
  HTTP.MediaType ->
  h (Linked ts Response, body) (Linked (RequiredHeader "Content-Type" Text : Body body : ts) Response)
setBody mediaType = proc (r, body) -> do
  r' <- plant (Body (Just mediaType)) -< (r, body)
  let mt = decodeUtf8 $ HTTP.renderHeader mediaType
  returnA <<< plant Header -< (r', mt)

setBodyWithoutContentType ::
  forall body h ts.
  Set h (Body body) Response =>
  h (Linked ts Response, body) (Linked (Body body : ts) Response)
setBodyWithoutContentType = plant (Body Nothing)

setJSONBody' ::
  forall body h ts.
  Sets h [JSONBody body, RequiredHeader "Content-Type" Text] Response =>
  HTTP.MediaType ->
  h (Linked ts Response, body) (Linked (RequiredHeader "Content-Type" Text : JSONBody body : ts) Response)
setJSONBody' mediaType = proc (r, body) -> do
  r' <- plant (JSONBody (Just mediaType)) -< (r, body)
  let mt = decodeUtf8 $ HTTP.renderHeader mediaType
  returnA <<< plant Header -< (r', mt)

setJSONBody ::
  forall body h ts.
  Sets h [JSONBody body, RequiredHeader "Content-Type" Text] Response =>
  h (Linked ts Response, body) (Linked (RequiredHeader "Content-Type" Text : JSONBody body : ts) Response)
setJSONBody = setJSONBody' "application/json"

setJSONBodyWithoutContentType ::
  forall body h ts.
  Set h (JSONBody body) Response =>
  h (Linked ts Response, body) (Linked (JSONBody body : ts) Response)
setJSONBodyWithoutContentType = plant (JSONBody Nothing)

respondA ::
  forall body h.
  Sets h [Status, Body body, RequiredHeader "Content-Type" Text] Response =>
  HTTP.Status ->
  HTTP.MediaType ->
  h body (Linked [RequiredHeader "Content-Type" Text, Body body, Status] Response)
respondA status mediaType = proc body -> do
  r <- mkResponse status -< ()
  setBody mediaType -< (r, body)

respondJsonA ::
  forall body h.
  Sets h [Status, JSONBody body, RequiredHeader "Content-Type" Text] Response =>
  HTTP.Status ->
  h body (Linked [RequiredHeader "Content-Type" Text, JSONBody body, Status] Response)
respondJsonA status = respondJsonA' status "application/json"

respondJsonA' ::
  forall body h.
  Sets h [Status, JSONBody body, RequiredHeader "Content-Type" Text] Response =>
  HTTP.Status ->
  HTTP.MediaType ->
  h body (Linked [RequiredHeader "Content-Type" Text, JSONBody body, Status] Response)
respondJsonA' status mediaType = proc body -> do
  r <- mkResponse status -< ()
  setJSONBody' mediaType -< (r, body)
