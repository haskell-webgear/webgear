module Unit.Trait.Header (
  tests,
) where

import Data.Functor.Identity (runIdentity)
import Network.Wai (defaultRequest, requestHeaders)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebGear.Core.Request (Request (..))
import WebGear.Core.Trait (getTrait, linkzero)
import WebGear.Core.Trait.Header (Header (..), HeaderNotFound (..), RequiredHeader)
import WebGear.Server.Handler (runServerHandler)
import WebGear.Server.Trait.Header ()

testMissingHeaderFails :: TestTree
testMissingHeaderFails = testCase "Missing header fails Header trait" $ do
  let req = linkzero $ Request $ defaultRequest{requestHeaders = []}
  runIdentity $ do
    res <- runServerHandler (getTrait (Header :: RequiredHeader "foo" Int)) [""] req
    pure $ case res of
      Right (Left e) -> e @?= Left HeaderNotFound
      _ -> assertFailure "unexpected success"

tests :: TestTree
tests = testGroup "Trait.Header" [testMissingHeaderFails]
