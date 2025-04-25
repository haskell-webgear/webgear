module Scotty (application) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Network.HTTP.Types as HTTP
import Network.Wai
import Web.Scotty

application :: IO Application
application = scottyApp $ do
  get "/hello" $ do
    setHeader "Content-Type" "text/plain"
    text "Hello World"

  get "/path-var/:word/:count" $ do
    word <- pathParam "word"
    count <- pathParam "count"
    setHeader "Content-Type" "text/plain"
    text $ LText.intercalate "," $ replicate count word

  get "/query-param" $ do
    word <- queryParam "word"
    count <- queryParam "count"
    setHeader "Content-Type" "text/plain"
    text $ LText.intercalate "," $ replicate count word

  get "/header" $
    header "APIKey" >>= \case
      Just apiKey -> do
        setHeader "Content-Type" "text/plain"
        text apiKey
      Nothing ->
        status HTTP.badRequest400

  put "/upload" $ do
    b <- body
    setHeader "Content-Type" "text/plain"
    if b == expectedUploadBody
      then text "done"
      else text "not done"

  post "/download/:input" $ do
    input <- pathParam "input"
    setHeader "Content-Type" "text/plain"
    text $ LText.decodeUtf8 $ LBS.replicate (1024 * 1024) input

expectedUploadBody :: LBS.ByteString
expectedUploadBody = LBS.replicate (1024 * 1024) 1
