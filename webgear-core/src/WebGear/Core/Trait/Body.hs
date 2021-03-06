{- | Traits and middlewares to handle request and response body
   payloads.

 There are a number of ways to extract a body from a request:

 The 'requestBody' middleware attempts to convert the body to a
 Haskell value or invoke an error handler if that fails.

 The 'jsonRequestBody' middleware attempts to convert a JSON
 formatted body to a Haskell value or invoke an error handler if that
 fails. It uses the standard "application/json" media type.

 The 'jsonRequestBody'' middleware is similar but supports custom
 media types.

 Similarly, there are a number of ways to set a response body:

 The easiest option is to use one of 'respondA', 'respondJsonA', or
 'respondJsonA'' middlewares. These middlewares generate a response
 from an HTTP status and a response body.

 If you need finer control over setting the body, use one of
 'setBody', 'setBodyWithoutContentType', 'setJSONBody',
 'setJSONBodyWithoutContentType', or 'setJSONBody''. These arrows
 accept a linked response and a body and sets the body in the
 response. You can generate an input response object using functions
 from "WebGear.Core.Trait.Status" module.
-}
module WebGear.Core.Trait.Body (
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

import Control.Arrow (ArrowChoice)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Media as HTTP
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Middleware)
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get, Linked, Set, Sets, Trait (..), TraitAbsence (..), plant, probe)
import WebGear.Core.Trait.Header (Header (..), RequiredHeader)
import WebGear.Core.Trait.Status (Status, mkResponse)

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
  forall body a h ts.
  Sets h [Body body, RequiredHeader "Content-Type" Text] Response =>
  -- | The media type of the response body
  HTTP.MediaType ->
  h a (Linked ts Response) ->
  h (body, a) (Linked (RequiredHeader "Content-Type" Text : Body body : ts) Response)
setBody mediaType prevHandler = proc (body, a) -> do
  r <- prevHandler -< a
  r' <- plant (Body (Just mediaType)) -< (r, body)
  let mt = decodeUtf8 $ HTTP.renderHeader mediaType
  plant Header -< (r', mt)

-- | Set the response body without specifying any media type.
setBodyWithoutContentType ::
  forall body a h ts.
  Set h (Body body) Response =>
  h a (Linked ts Response) ->
  h (body, a) (Linked (Body body : ts) Response)
setBodyWithoutContentType prevHandler = proc (body, a) -> do
  r <- prevHandler -< a
  plant (Body Nothing) -< (r, body)

{- | Set the response body to a JSON value along with a media type.

  The media type value is used to set the "Content-Type" header in the response.
-}
setJSONBody' ::
  forall body a h ts.
  Sets h [JSONBody body, RequiredHeader "Content-Type" Text] Response =>
  -- | The media type of the response body
  HTTP.MediaType ->
  h a (Linked ts Response) ->
  h (body, a) (Linked (RequiredHeader "Content-Type" Text : JSONBody body : ts) Response)
setJSONBody' mediaType prevHandler = proc (body, a) -> do
  r <- prevHandler -< a
  r' <- plant (JSONBody (Just mediaType)) -< (r, body)
  let mt = decodeUtf8 $ HTTP.renderHeader mediaType
  plant Header -< (r', mt)

{- | Set the response body to a JSON value.

  The "Content-Type" header will be set to "application/json".
-}
setJSONBody ::
  forall body a h ts.
  Sets h [JSONBody body, RequiredHeader "Content-Type" Text] Response =>
  h a (Linked ts Response) ->
  h (body, a) (Linked (RequiredHeader "Content-Type" Text : JSONBody body : ts) Response)
setJSONBody = setJSONBody' "application/json"

{- | Set the response body to a JSON value without specifying any
 media type.
-}
setJSONBodyWithoutContentType ::
  forall body a h ts.
  Set h (JSONBody body) Response =>
  h a (Linked ts Response) ->
  h (body, a) (Linked (JSONBody body : ts) Response)
setJSONBodyWithoutContentType prevHandler = proc (body, a) -> do
  r <- prevHandler -< a
  plant (JSONBody Nothing) -< (r, body)

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
respondA status mediaType = proc body ->
  setBody mediaType (mkResponse status) -< (body, ())

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
respondJsonA' status mediaType = proc body ->
  setJSONBody' mediaType (mkResponse status) -< (body, ())
