-- | Parsing and rendering of MIME types
module WebGear.Server.MIMETypes (
  -- * Parsing and rendering MIME types
  BodyUnrender (..),
  BodyRender (..),

  -- * FormData utils
  inMemoryBackend,
  tempFileBackend,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (MonadResource, getInternalState, liftResourceT)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor (first))
import qualified Data.Binary.Builder as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack)
import Data.Text.Conversions (FromText (..), ToText (..))
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Network.HTTP.Media as HTTP
import Network.Wai.Parse (BackEnd, lbsBackEnd, parseRequestBodyEx, tempFileBackEnd)
import Web.FormUrlEncoded (
  FromForm (..),
  ToForm (..),
  urlDecodeForm,
  urlEncodeFormStable,
 )
import WebGear.Core.MIMETypes (
  FormData (..),
  FormDataResult (..),
  FormURLEncoded (..),
  HTML,
  JSON,
  MIMEType (..),
  OctetStream,
  PlainText,
 )
import WebGear.Core.Request (Request (..), getRequestBody)
import WebGear.Core.Response (Response, ResponseBody (..))

{- | Instances of this class are used to parse the body of a request
as specified by a MIME type.
-}
class (MIMEType mt) => BodyUnrender m mt a where
  {- | Parse a request body. Return a 'Left' value with error messages
  in case of failure.
  -}
  bodyUnrender :: mt -> Request -> m (Either Text a)

{- | Instances of this class serializes a value to a response body as
specified by a MIME type.
-}
class (MIMEType mt) => BodyRender m mt a where
  {- | Render a value in the format specified by the media type.

  Returns the response body and the media type to be used in the
  "Content-Type" header. This could be a variant of the original
  media type with additional parameters.
  -}
  bodyRender :: mt -> Response -> a -> m (HTTP.MediaType, ResponseBody)

--------------------------------------------------------------------------------

instance (MonadIO m, FromForm a) => BodyUnrender m FormURLEncoded a where
  bodyUnrender :: FormURLEncoded -> Request -> m (Either Text a)
  bodyUnrender FormURLEncoded request = do
    body <- liftIO $ getRequestBody request
    pure $ urlDecodeForm body >>= fromForm

instance (Monad m, ToForm a) => BodyRender m FormURLEncoded a where
  bodyRender :: FormURLEncoded -> Response -> a -> m (HTTP.MediaType, ResponseBody)
  bodyRender FormURLEncoded _response a = do
    let body = ResponseBodyBuilder $ B.fromLazyByteString $ urlEncodeFormStable $ toForm a
    pure (mimeType FormURLEncoded, body)

--------------------------------------------------------------------------------

instance (MonadIO m) => BodyUnrender m HTML BS.ByteString where
  bodyUnrender :: HTML -> Request -> m (Either Text BS.ByteString)
  bodyUnrender _ request = do
    body <- liftIO $ getRequestBody request
    pure $ Right $ LBS.toStrict body

instance (Monad m) => BodyRender m HTML BS.ByteString where
  bodyRender :: HTML -> Response -> BS.ByteString -> m (HTTP.MediaType, ResponseBody)
  bodyRender html _response a = do
    let body = ResponseBodyBuilder $ B.fromByteString a
    pure (mimeType html, body)

instance (MonadIO m) => BodyUnrender m HTML LBS.ByteString where
  bodyUnrender :: HTML -> Request -> m (Either Text LBS.ByteString)
  bodyUnrender _ request = do
    body <- liftIO $ getRequestBody request
    pure $ Right body

instance (Monad m) => BodyRender m HTML LBS.ByteString where
  bodyRender :: HTML -> Response -> LBS.ByteString -> m (HTTP.MediaType, ResponseBody)
  bodyRender html _response a = do
    let body = ResponseBodyBuilder $ B.fromLazyByteString a
    pure (mimeType html, body)

--------------------------------------------------------------------------------

instance (MonadIO m, Aeson.FromJSON a) => BodyUnrender m JSON a where
  bodyUnrender :: JSON -> Request -> m (Either Text a)
  bodyUnrender _ request = do
    s <- liftIO $ getRequestBody request
    pure $ first pack $ Aeson.eitherDecode s

instance (Monad m, Aeson.ToJSON a) => BodyRender m JSON a where
  bodyRender :: JSON -> Response -> a -> m (HTTP.MediaType, ResponseBody)
  bodyRender json _response a = do
    let body = ResponseBodyBuilder $ Aeson.fromEncoding $ Aeson.toEncoding a
    pure (mimeType json, body)

--------------------------------------------------------------------------------

-- | A backend that stores all files in memory
inMemoryBackend :: BackEnd LBS.ByteString
inMemoryBackend = lbsBackEnd

-- | A backend that stores files in a temp directory.
tempFileBackend :: (MonadResource m) => m (BackEnd FilePath)
tempFileBackend = do
  st <- liftResourceT getInternalState
  pure $ tempFileBackEnd st

instance (MonadIO m) => BodyUnrender m (FormData m a) (FormDataResult a) where
  bodyUnrender :: FormData m a -> Request -> m (Either Text (FormDataResult a))
  bodyUnrender FormData{parseOptions, backendOptions} request = do
    (formDataParams, formDataFiles) <- do
      be <- backendOptions
      liftIO $ parseRequestBodyEx parseOptions be $ toWaiRequest request
    pure $ Right FormDataResult{formDataParams, formDataFiles}

--------------------------------------------------------------------------------

instance (MonadIO m) => BodyUnrender m OctetStream BS.ByteString where
  bodyUnrender :: OctetStream -> Request -> m (Either Text BS.ByteString)
  bodyUnrender _ request = do
    body <- liftIO $ getRequestBody request
    pure $ Right $ LBS.toStrict body

instance (Monad m) => BodyRender m OctetStream BS.ByteString where
  bodyRender :: OctetStream -> Response -> BS.ByteString -> m (HTTP.MediaType, ResponseBody)
  bodyRender os _response a = do
    let body = ResponseBodyBuilder $ B.fromByteString a
    pure (mimeType os, body)

instance (MonadIO m) => BodyUnrender m OctetStream LBS.ByteString where
  bodyUnrender :: OctetStream -> Request -> m (Either Text LBS.ByteString)
  bodyUnrender _ request = do
    body <- liftIO $ getRequestBody request
    pure $ Right body

instance (Monad m) => BodyRender m OctetStream LBS.ByteString where
  bodyRender :: OctetStream -> Response -> LBS.ByteString -> m (HTTP.MediaType, ResponseBody)
  bodyRender os _response a = do
    let body = ResponseBodyBuilder $ B.fromLazyByteString a
    pure (mimeType os, body)

--------------------------------------------------------------------------------

instance (MonadIO m, FromText a) => BodyUnrender m PlainText a where
  bodyUnrender :: PlainText -> Request -> m (Either Text a)
  bodyUnrender _ request = do
    body <- liftIO $ getRequestBody request
    pure $ case LText.decodeUtf8' body of
      Left e -> Left $ pack $ show e
      Right t -> Right $ fromText $ LText.toStrict t

instance (Monad m, ToText a) => BodyRender m PlainText a where
  bodyRender :: PlainText -> Response -> a -> m (HTTP.MediaType, ResponseBody)
  bodyRender txt _response a = do
    let body = ResponseBodyBuilder $ Text.encodeUtf8Builder $ toText a
    pure (mimeType txt, body)
