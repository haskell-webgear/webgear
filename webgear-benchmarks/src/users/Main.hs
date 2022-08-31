{-# OPTIONS_GHC -Wno-error=deprecations #-}

module Main where

import Criterion.Main (bench, bgroup, defaultMain, nfIO)
import Data.ByteString (ByteString)
import Data.IORef (newIORef, readIORef, writeIORef)
import Network.HTTP.Types (methodDelete, methodGet, methodPut, statusCode)
import Network.Wai (Application, defaultRequest)
import Network.Wai.Internal (Request (..), Response (..), ResponseReceived (..))
import System.Environment (getArgs)

import qualified Scotty
import qualified Servant
import qualified WebGear

import Control.Monad
import Model (newStore)

main :: IO ()
main = do
  store <- newStore

  let mkBenchmark name test =
        bgroup name $
          map (\count -> bench (show count) $ nfIO $ test count) [500, 1000, 5000]

  let webgear = mkBenchmark "webgear" (\n -> runTest n $ WebGear.application store)
  let servant = mkBenchmark "servant" (\n -> runTest n $ Servant.application store)
  let scotty = mkBenchmark "scotty" (\n -> Scotty.application store >>= runTest n)

  getArgs >>= \case
    ["webgear"] -> defaultMain [webgear]
    ["servant"] -> defaultMain [servant]
    ["scotty"] -> defaultMain [scotty]
    _ -> defaultMain [webgear, servant, scotty]

runTest :: Int -> Application -> IO ()
runTest count app = replicateM_ count $ do
  _ <- app getRequest (respond 404)
  _ <- putRequest >>= flip app (respond 200)
  _ <- app getRequest (respond 200)
  _ <- app deleteRequest (respond 204)
  return ()

putRequest :: IO Request
putRequest = do
  f <- bodyGetter "{\"userId\": 1, \"userName\": \"John Doe\", \"dateOfBirth\": \"2000-03-01\", \"gender\": \"Male\", \"emailAddress\": \"john@example.com\"}"
  return
    defaultRequest
      { requestMethod = methodPut
      , requestHeaders = [("Content-type", "application/json")]
      , pathInfo = ["v1", "users", "1"]
      , requestBody = f
      }

bodyGetter :: ByteString -> IO (IO ByteString)
bodyGetter s = do
  ref <- newIORef (Just s)
  pure $
    readIORef ref >>= \case
      Nothing -> pure ""
      Just x -> writeIORef ref Nothing >> return x

getRequest :: Request
getRequest =
  defaultRequest
    { requestMethod = methodGet
    , pathInfo = ["v1", "users", "1"]
    }

deleteRequest :: Request
deleteRequest =
  defaultRequest
    { requestMethod = methodDelete
    , pathInfo = ["v1", "users", "1"]
    }

respond :: Int -> Response -> IO ResponseReceived
respond expectedStatus res = do
  let actualStatus = statusOf res
  when (expectedStatus /= actualStatus) $
    putStrLn "Unexpected response status"
  return ResponseReceived

statusOf :: Response -> Int
statusOf (ResponseFile status _ _ _) = statusCode status
statusOf (ResponseBuilder status _ _) = statusCode status
statusOf (ResponseStream status _ _) = statusCode status
statusOf (ResponseRaw _ res) = statusOf res
