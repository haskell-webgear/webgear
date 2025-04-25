{-# OPTIONS_GHC -Wno-error=deprecations #-}

module Main where

import Control.Monad
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nfIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.Text as Text
import Data.Word (Word8)
import Network.HTTP.Types (methodGet, methodPost, methodPut, statusCode)
import Network.Wai (Application, defaultRequest)
import Network.Wai.Internal (Request (..), Response (..), ResponseReceived (..))
import qualified Scotty
import qualified Servant
import qualified WebGear

main :: IO ()
main = do
  scotty <- Scotty.application

  let mkBenchmark :: (String, Application -> IO ()) -> Benchmark
      mkBenchmark (name, test) =
        bgroup name $
          map
            (\(appName, app) -> bench appName $ nfIO $ test app)
            [ ("Scotty", scotty)
            , ("Servant", Servant.application)
            , ("WebGear", WebGear.application)
            ]

  defaultMain $
    map
      mkBenchmark
      [ ("hello-world", benchHelloWorld)
      , ("path-variable", benchPathVar)
      , ("query-param", benchQueryParam)
      , ("header", benchHeader)
      , ("upload", benchUpload)
      , ("download", benchDownload)
      ]

benchHelloWorld :: Application -> IO ()
benchHelloWorld app = replicateM_ 50 $ do
  let request =
        defaultRequest
          { requestMethod = methodGet
          , pathInfo = ["hello"]
          }
  app request (validateResponse "Hello World")

benchPathVar :: Application -> IO ()
benchPathVar app = replicateM_ 50 $ do
  let request =
        defaultRequest
          { requestMethod = methodGet
          , pathInfo = ["path-var", "word", "10"]
          }
  app request (validateResponse "word,word,word,word,word,word,word,word,word,word")

benchQueryParam :: Application -> IO ()
benchQueryParam app = replicateM_ 50 $ do
  let request =
        defaultRequest
          { requestMethod = methodGet
          , pathInfo = ["query-param"]
          , queryString = [("word", Just "word"), ("count", Just "10")]
          }
  app request (validateResponse "word,word,word,word,word,word,word,word,word,word")

benchHeader :: Application -> IO ()
benchHeader app = replicateM_ 50 $ do
  let request =
        defaultRequest
          { requestMethod = methodGet
          , pathInfo = ["header"]
          , requestHeaders = [("APIKey", "secure-api-key")]
          }
  app request (validateResponse "secure-api-key")

benchUpload :: Application -> IO ()
benchUpload app = replicateM_ 10 $ do
  bRef <- newIORef True
  let request =
        defaultRequest
          { requestMethod = methodPut
          , pathInfo = ["upload"]
          , requestHeaders = [("Content-type", "application/octet-stream")]
          , requestBody
          }

      requestBody :: IO ByteString
      requestBody = atomicModifyIORef' bRef $ \b -> do
        if b
          then (False, BS.replicate (1024 * 1024) 1)
          else (False, BS.empty)

  app request (validateResponse "done")

benchDownload :: Application -> IO ()
benchDownload app = replicateM_ 10 $ do
  let request =
        defaultRequest
          { requestMethod = methodPost
          , pathInfo = ["download", Text.pack (show downloadInput)]
          }

      validateDownloadResponse :: Response -> IO ResponseReceived
      validateDownloadResponse (ResponseBuilder status _ body) = do
        when (statusCode status /= 200) $
          putStrLn "Incorrect response code"
        when (toLazyByteString body /= expectedDownloadResponseBody) $
          putStrLn ("Incorrect response body: " <> show body)
        pure ResponseReceived
      validateDownloadResponse _ = do
        putStrLn "Incorrect response type"
        pure ResponseReceived

  app request validateDownloadResponse

downloadInput :: Word8
downloadInput = 1

expectedDownloadResponseBody :: LBS.ByteString
expectedDownloadResponseBody = LBS.replicate (1024 * 1024) downloadInput

validateResponse :: LBS.ByteString -> Response -> IO ResponseReceived
validateResponse expectedBody (ResponseBuilder status _ body) = do
  when (statusCode status /= 200) $
    putStrLn "Incorrect response code"
  when (toLazyByteString body /= expectedBody) $
    putStrLn ("Incorrect response body: " <> show body)
  pure ResponseReceived
validateResponse _ _ = do
  putStrLn "Incorrect response type"
  pure ResponseReceived
