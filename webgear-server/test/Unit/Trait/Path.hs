module Unit.Trait.Path (
  tests,
) where

import Data.Functor.Identity (runIdentity)
import Network.Wai (defaultRequest, pathInfo)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import WebGear.Core.Request (Request (..))
import WebGear.Core.Trait (getTrait, wzero)
import WebGear.Core.Trait.Path (PathVar (..), PathVarError (..))
import WebGear.Server.Handler (runServerHandler)
import WebGear.Server.Trait.Path ()

testMissingPathVar :: TestTree
testMissingPathVar = testCase "PathVar match: missing variable" $ do
  let req = wzero $ Request $ defaultRequest{pathInfo = []}
  runIdentity $ do
    res <- runServerHandler (getTrait (PathVar @"tag" @Int)) [] req
    pure $ case res of
      Right (Left e) -> e @?= PathVarNotFound
      _ -> assertFailure "unexpected success"

tests :: TestTree
tests = testGroup "Trait.Path" [testMissingPathVar]
