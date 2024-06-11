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
  UnknownContentBody (..),

  -- * Middlewares
  requestBody,
  respondA,
  setBody,
  setBodyWithoutContentType,
) where

import Control.Arrow ((<<<))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Media as HTTP
import qualified Network.HTTP.Types as HTTP
import WebGear.Core.Handler (Handler (..), Middleware, unwitnessA)
import WebGear.Core.MIMETypes (MIMEType (..))
import WebGear.Core.Request (Request)
import WebGear.Core.Response (Response, ResponseBody)
import WebGear.Core.Trait (
  Get,
  Prerequisite,
  Set,
  Sets,
  Attribute,
  Absence,
  With (..),
  plant,
  probe,
 )
import WebGear.Core.Trait.Header (RequiredResponseHeader, ResponseHeader (..))
import WebGear.Core.Trait.Status (Status, mkResponse)

-- | Request or response body with MIME types @mimeTypes@ and type @t@.
newtype Body (mimeType :: Type) (t :: Type) = Body mimeType

type instance Attribute (Body mt t) Request = t

type instance Absence (Body mt t) Request = Text

type instance Prerequisite (Body mt t) ts Request = ()

type instance Attribute (Body mt t) Response = t

-- | Type representing responses without a statically known MIME type
data UnknownContentBody = UnknownContentBody

type instance Attribute UnknownContentBody Response = ResponseBody

{- | Middleware to extract a request body.

 The @nextHandler@ is invoked after successfully extracting the body
 and the @errorHandler@ is invoked when there is an error.

 Usage:

@
 requestBody \@'Text' 'WebGear.Core.MIMETypes.PlainText' errorHandler nextHandler
@
-}
requestBody ::
  forall t mt h m ts.
  ( Handler h m
  , Get h (Body mt t) Request
  ) =>
  mt ->
  -- | Error handler in case body cannot be retrieved
  h (Request `With` ts, Text) Response ->
  Middleware h ts (Body mt t : ts)
requestBody mt errorHandler nextHandler = proc request -> do
  result <- probe (Body mt) -< request
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
 response' <- setBody 'WebGear.Core.MIMETypes.JSON' -< (response, body)
@
-}
setBody ::
  forall body mt h ts.
  ( Sets h [Body mt body, RequiredResponseHeader "Content-Type" Text] Response
  , MIMEType mt
  ) =>
  mt ->
  h (Response `With` ts, body) (Response `With` (Body mt body : RequiredResponseHeader "Content-Type" Text : ts))
setBody mt = proc (response, body) -> do
  let ct = mimeType mt
  response' <- plant ResponseHeader -< (response, decodeUtf8 $ HTTP.renderHeader ct)
  plant (Body mt) -< (response', body)
{-# INLINE setBody #-}

{- | Set the response body without specifying any media type.

Usage:

@
 let body :: ResponseBody = ...
 response' <- setBodyWithoutContentType -< (response, body)
@
-}
setBodyWithoutContentType ::
  forall h ts.
  (Set h UnknownContentBody Response) =>
  h (Response `With` ts, ResponseBody) (Response `With` (UnknownContentBody : ts))
setBodyWithoutContentType = plant UnknownContentBody
{-# INLINE setBodyWithoutContentType #-}

{- | A convenience arrow to generate a response specifying a status and body.

 The "Content-Type" header will be set to the value specified by @mt@.

 Usage:

@
 let body :: SomeJSONType = ...
 respondA 'HTTP.ok200' 'WebGear.Core.MIMETypes.JSON' -< body
@
-}
respondA ::
  forall body mt h m.
  ( Handler h m
  , Sets h [Status, Body mt body, RequiredResponseHeader "Content-Type" Text] Response
  , MIMEType mt
  ) =>
  -- | Response status
  HTTP.Status ->
  mt ->
  h body Response
respondA status mt = proc body -> do
  response <- mkResponse status -< ()
  unwitnessA <<< setBody mt -< (response, body)
{-# INLINE respondA #-}
