{- | Traits and middlewares to handle request and response body
   payloads.

 The 'requestBody' middleware attempts to convert the body to a
 Haskell value or invoke an error handler if that fails.

 The 'respondA' middleware generates a response from an HTTP status
 and a response body.

 If you need finer control over setting the body, use 'setBody' or
 'setBodyWithoutContentType'. These arrows accept a witnessed response
 and a body and sets the body in the response. You can generate an
 input response object using functions from
 "WebGear.Core.Trait.Status" module.
-}
module WebGear.Core.Trait.Body (
  -- * Traits
  Body (..),

  -- * Middlewares
  requestBody,
  respondA,
  setBody,
  setBodyWithoutContentType,
) where

import Control.Arrow (ArrowChoice, (<<<))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Media as HTTP
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Handler, Middleware, unwitnessA)
import WebGear.Core.MIMEType (MIMEType (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response)
import WebGear.Core.Trait (Get, Set, Sets, Trait (..), TraitAbsence (..), With, plant, probe)
import WebGear.Core.Trait.Header (RequiredResponseHeader, ResponseHeader (..))
import WebGear.Core.Trait.Status (Status, mkResponse)

-- | Request or response body with MIME types @mts@ and type @t@.
data Body (mts :: [Type]) (t :: Type) = Body

instance Trait (Body mts t) Request where
  type Attribute (Body mts t) Request = t

instance TraitAbsence (Body mts t) Request where
  type Absence (Body mts t) Request = Text

instance Trait (Body mts t) Response where
  type Attribute (Body mts t) Response = t

{- | Middleware to extract a request body.

 The @nextHandler@ is invoked after successfully extracting the body
 and the @errorHandler@ is invoked when there is an error.

 Usage:

@
 requestBody \@'['PlainText'] \@'Text' errorHandler nextHandler
@
-}
requestBody ::
  forall mts t h req.
  (Get h (Body mts t) Request, ArrowChoice h) =>
  -- | Error handler in case body cannot be retrieved
  h (Request `With` req, Text) Response ->
  Middleware h req (Body mts t : req)
requestBody errorHandler nextHandler = proc request -> do
  result <- probe Body -< request
  case result of
    Left err -> errorHandler -< (request, err)
    Right t -> nextHandler -< t
{-# INLINE requestBody #-}

{- | Set the response body along with a media type.

 The MIME type @mt@ is used to set the "Content-Type" header in the
 response.

 Usage:

@
 let body :: SomeJSONType = ...
 response' <- setBody \@'JSON' -< (response, body)
@
-}
setBody ::
  forall mt body h res.
  (Sets h [Body '[mt] body, RequiredResponseHeader "Content-Type" Text] Response, MIMEType mt) =>
  h (Response `With` res, body) (Response `With` (RequiredResponseHeader "Content-Type" Text : Body '[mt] body : res))
setBody = proc (response, body) -> do
  response' <- plant Body -< (response, body)
  let mt = decodeUtf8 $ HTTP.renderHeader $ mimeType $ Proxy @mt
  plant ResponseHeader -< (response', mt)
{-# INLINE setBody #-}

{- | Set the response body without specifying any media type.

Usage:

@
 let body :: SomeType = ...
 response' <- setBodyWithoutContentType -< (response, body)
@
-}
setBodyWithoutContentType ::
  forall body h res.
  (Set h (Body '[] body) Response) =>
  h (Response `With` res, body) (Response `With` (Body '[] body : res))
setBodyWithoutContentType = plant Body
{-# INLINE setBodyWithoutContentType #-}

{- | A convenience arrow to generate a response specifying a status and body.

 The "Content-Type" header will be set to the value specified by @mt@.

 Usage:

@
 let body :: SomeJSONType = ...
 respondA 'HTTP.ok200' (Proxy \@'JSON') -< body
@
-}
respondA ::
  forall mt body h m.
  ( Handler h m
  , Sets h [Status, Body '[mt] body, RequiredResponseHeader "Content-Type" Text] Response
  , MIMEType mt
  ) =>
  -- | Response status
  HTTP.Status ->
  Proxy mt ->
  h body Response
respondA status _ = proc body -> do
  response <- mkResponse status -< ()
  unwitnessA <<< setBody @mt -< (response, body)
{-# INLINE respondA #-}
