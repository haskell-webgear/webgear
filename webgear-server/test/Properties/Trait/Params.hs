module Properties.Trait.Params (
  tests,
) where

import Data.Functor.Identity (runIdentity)
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai (defaultRequest, queryString)
import Test.QuickCheck (Property, allProperties, counterexample, property, (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import WebGear.Core.Middleware.QueryParam (ParamParseError (..), QueryParam (..))
import WebGear.Core.Modifiers (Existence (..), ParseStyle (..))
import WebGear.Core.Request (Request (..))
import WebGear.Core.Trait (getTrait, linkzero)
import WebGear.Server.Handler (runServerHandler)
import WebGear.Server.Middleware.QueryParam ()

prop_paramParseError :: Property
prop_paramParseError = property $ \hval ->
  let hval' = "test-" <> hval
      req = linkzero $ Request $ defaultRequest{queryString = [("foo", Just $ encodeUtf8 hval')]}
   in runIdentity $ do
        res <- runServerHandler (getTrait (QueryParam :: QueryParam Required Strict "foo" Int)) [""] req
        pure $ case res of
          Right (Left e) ->
            e === Right (ParamParseError $ "could not parse: `" <> hval' <> "' (input does not start with a digit)")
          v -> counterexample ("Unexpected result: " <> show v) (property False)

prop_paramParseSuccess :: Property
prop_paramParseSuccess = property $ \(n :: Int) ->
  let req = linkzero $ Request $ defaultRequest{queryString = [("foo", Just $ fromString $ show n)]}
   in runIdentity $ do
        res <- runServerHandler (getTrait (QueryParam :: QueryParam Required Strict "foo" Int)) [""] req
        pure $ case res of
          Right (Right n') -> n === n'
          e -> counterexample ("Unexpected result: " <> show e) (property False)

-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Params" $allProperties
