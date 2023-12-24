{-# OPTIONS_GHC -Wno-deprecations #-}

module Properties.Trait.Body (
  tests,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.String (fromString)
import Network.Wai (defaultRequest, requestBody)
import Test.QuickCheck (Property, allProperties, counterexample, property)
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Monadic (assert, monadicIO, monitor)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import WebGear.Core.MIMETypes (JSON (..))
import WebGear.Core.Request (Request (..))
import WebGear.Core.Trait (With, getTrait, wzero)
import WebGear.Core.Trait.Body (Body (..))
import WebGear.Server.Handler (runServerHandler)
import WebGear.Server.Trait.Body ()

jsonBody :: Body JSON t
jsonBody = Body JSON

bodyToRequest :: (MonadIO m, Show a) => a -> m (Request `With` '[])
bodyToRequest x = do
  body <- liftIO $ newIORef $ Just $ fromString $ show x
  let f = readIORef body >>= maybe (pure "") (\s -> writeIORef body Nothing >> pure s)
  return $ wzero $ Request $ defaultRequest{requestBody = f}

prop_emptyRequestBodyFails :: Property
prop_emptyRequestBodyFails = monadicIO $ do
  req <- bodyToRequest ("" :: String)
  runServerHandler (getTrait (jsonBody @Int)) [""] req >>= \case
    Right (Left _) -> assert True
    e -> monitor (counterexample $ "Unexpected " <> show e) >> assert False

prop_validBodyParses :: Property
prop_validBodyParses = property $ \n -> monadicIO $ do
  req <- bodyToRequest (n :: Integer)
  runServerHandler (getTrait jsonBody) [""] req >>= \case
    Right (Right n') -> assert (n == n')
    _ -> assert False

prop_invalidBodyTypeFails :: Property
prop_invalidBodyTypeFails = property $ \n -> monadicIO $ do
  req <- bodyToRequest (n :: Integer)
  runServerHandler (getTrait (jsonBody @String)) [""] req >>= \case
    Right (Left _) -> assert True
    _ -> assert False

-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Body" $allProperties
