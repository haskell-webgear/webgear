module WebGear (application) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import Data.Word (Word8)
import Network.HTTP.Types (StdMethod (..))
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application)
import WebGear.Server

--------------------------------------------------------------------------------
-- The application server
--------------------------------------------------------------------------------
application :: Application
application =
  toApplication $
    [route| GET /hello |] helloWorldHandler
      <+> [route| GET /path-var/word:LText.Text/count:Int |] pathVarHandler
      <+> [route| GET /query-param |] queryParamHandler
      <+> [route| GET /header |] headerHandler
      <+> [route| PUT /upload |] uploadHandler
      <+> [route| POST /download/input:Word8 |] downloadHandler

helloWorldHandler :: (StdHandler h IO) => h (Request `With` ts) Response
helloWorldHandler = proc _request -> do
  respondA @LText.Text HTTP.ok200 PlainText -< "Hello World"

pathVarHandler ::
  ( StdHandler h IO
  , HaveTraits [PathVar "word" LText.Text, PathVar "count" Int] ts
  ) =>
  h (Request `With` ts) Response
pathVarHandler = proc request -> do
  let word = pick @(PathVar "word" LText.Text) $ from request
      count = pick @(PathVar "count" Int) $ from request
  respondA HTTP.ok200 PlainText -< LText.intercalate "," $ replicate count word

queryParamHandler ::
  ( StdHandler h IO
  , Gets h [RequiredQueryParam "word" LText.Text, RequiredQueryParam "count" Int]
  ) =>
  h (Request `With` ts) Response
queryParamHandler =
  queryParam @"word" @LText.Text badRequest $
    queryParam @"count" @Int badRequest $
      proc request -> do
        let word = pick @(RequiredQueryParam "word" LText.Text) $ from request
            count = pick @(RequiredQueryParam "count" Int) $ from request
        respondA HTTP.ok200 PlainText -< LText.intercalate "," $ replicate count word
  where
    badRequest = proc _ -> do
      respondA @LText.Text HTTP.badRequest400 PlainText -< "Missing query param"

headerHandler ::
  ( StdHandler h IO
  , Get h (RequiredRequestHeader "APIKey" LText.Text)
  ) =>
  h (Request `With` ts) Response
headerHandler =
  header @"APIKey" @LText.Text badRequest $
    proc request -> do
      let apiKey = pick @(RequiredRequestHeader "APIKey" LText.Text) $ from request
      respondA HTTP.ok200 PlainText -< apiKey
  where
    badRequest = proc _ -> do
      respondA @LText.Text HTTP.badRequest400 PlainText -< "Missing header"

uploadHandler ::
  ( StdHandler h IO
  , Get h (Body OctetStream ByteString)
  ) =>
  h (Request `With` ts) Response
uploadHandler =
  requestBody @ByteString OctetStream badRequest $
    proc request -> do
      let body = pick @(Body OctetStream ByteString) $ from request
      if body == expectedUploadBody
        then respondA @LText.Text HTTP.ok200 PlainText -< "done"
        else respondA @LText.Text HTTP.ok200 PlainText -< "not done"
  where
    badRequest = proc _ -> do
      respondA @LText.Text HTTP.badRequest400 PlainText -< "Missing request body"

expectedUploadBody :: ByteString
expectedUploadBody = BS.replicate (1024 * 1024) 1

downloadHandler ::
  ( StdHandler h IO
  , HasTrait (PathVar "input" Word8) ts
  ) =>
  h (Request `With` ts) Response
downloadHandler =
  proc request -> do
    let input = pick @(PathVar "input" Word8) $ from request
    respondA HTTP.ok200 PlainText -< LText.decodeUtf8 $ LBS.replicate (1024 * 1024) input
