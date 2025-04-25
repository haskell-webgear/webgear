module Servant (application) where

import Control.Monad.Except (throwError)
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy (Proxy (..))
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import Data.Word (Word8)
import Network.Wai
import Servant.API
import Servant.Server

type API =
  "hello" :> Get '[PlainText] LText.Text
    :<|> "path-var" :> Capture "word" LText.Text :> Capture "count" Int :> Get '[PlainText] LText.Text
    :<|> "query-param" :> QueryParam "word" LText.Text :> QueryParam "count" Int :> Get '[PlainText] LText.Text
    :<|> "header" :> Header "APIKey" LText.Text :> Get '[PlainText] LText.Text
    :<|> "upload" :> ReqBody '[OctetStream] LBS.ByteString :> Put '[PlainText] LText.Text
    :<|> "download" :> Capture "input" Word8 :> Post '[PlainText] LText.Text

application :: Application
application = serve (Proxy :: Proxy API) server

server :: Server API
server =
  helloWorldHandler
    :<|> pathVarHandler
    :<|> queryParamHandler
    :<|> headerHandler
    :<|> uploadHandler
    :<|> downloadHandler

helloWorldHandler :: Handler LText.Text
helloWorldHandler = pure "Hello World"

pathVarHandler :: LText.Text -> Int -> Handler LText.Text
pathVarHandler word count = pure $ LText.intercalate "," $ replicate count word

queryParamHandler :: Maybe LText.Text -> Maybe Int -> Handler LText.Text
queryParamHandler (Just word) (Just count) = pure $ LText.intercalate "," $ replicate count word
queryParamHandler _ _ = throwError err400

headerHandler :: Maybe LText.Text -> Handler LText.Text
headerHandler (Just word) = pure word
headerHandler Nothing = throwError err400

uploadHandler :: LBS.ByteString -> Handler LText.Text
uploadHandler body =
  if body == expectedUploadBody
    then pure "done"
    else pure "not done"

expectedUploadBody :: LBS.ByteString
expectedUploadBody = LBS.replicate (1024 * 1024) 1

downloadHandler :: Word8 -> Handler LText.Text
downloadHandler = pure . LText.decodeUtf8 . LBS.replicate (1024 * 1024)
