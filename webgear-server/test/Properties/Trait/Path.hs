module Properties.Trait.Path (
  tests,
) where

import Data.Functor.Identity (runIdentity)
import Data.String (fromString)
import Network.Wai (defaultRequest, pathInfo)
import Test.QuickCheck (Property, allProperties, property, (=/=), (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import WebGear.Core.Trait.Path (Path (..), PathVar (..), PathVarError (..))
import WebGear.Core.Request (Request (..))
import WebGear.Core.Trait (getTrait, linkzero)
import WebGear.Server.Handler (RoutePath (..), runServerHandler)
import WebGear.Server.Trait.Path ()

prop_pathMatch :: Property
prop_pathMatch = property $ \h ->
  let rest = ["foo", "bar"]
      req = linkzero $ Request $ defaultRequest{pathInfo = h : rest}
   in runIdentity $ do
        res <- runServerHandler (getTrait $ Path "a") (RoutePath $ h : rest) req
        pure $ case res of
          Right (Right _) -> h === "a"
          Right (Left _) -> h =/= "a"
          Left _ -> property False

prop_pathVarMatch :: Property
prop_pathVarMatch = property $ \(n :: Int) ->
  let rest = ["foo", "bar"]
      p = fromString (show n) : rest
      req = linkzero $ Request $ defaultRequest{pathInfo = p}
   in runIdentity $ do
        res <- runServerHandler (getTrait (PathVar @"tag" @Int)) (RoutePath p) req
        pure $ case res of
          Right (Right n') -> n' === n
          _ -> property False

prop_pathVarParseError :: Property
prop_pathVarParseError = property $ \(p, ps) ->
  let p' = "test-" <> p
      req = linkzero $ Request $ defaultRequest{pathInfo = p' : ps}
   in runIdentity $ do
        res <- runServerHandler (getTrait (PathVar @"tag" @Int)) (RoutePath $ p' : ps) req
        pure $ case res of
          Right (Left e) -> e === PathVarParseError ("could not parse: `" <> p' <> "' (input does not start with a digit)")
          _ -> property False

-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Path" $allProperties
