module Properties.Trait.Header (
  tests,
) where

import Data.Functor.Identity (runIdentity)
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import Network.Wai (defaultRequest, requestHeaders)
import Test.QuickCheck (Property, allProperties, counterexample, property, (===))
import Test.QuickCheck.Instances ()
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import WebGear.Core.Request (Request (..))
import WebGear.Core.Trait (getTrait, wzero)
import WebGear.Core.Trait.Header (Header (..), HeaderParseError (..), RequiredHeader)
import WebGear.Server.Handler (runServerHandler)
import WebGear.Server.Trait.Header ()

prop_headerParseError :: Property
prop_headerParseError = property $ \hval ->
  let hval' = "test-" <> hval
      req = wzero $ Request $ defaultRequest{requestHeaders = [("foo", encodeUtf8 hval')]}
   in runIdentity $ do
        res <- runServerHandler (getTrait (Header :: RequiredHeader "foo" Int)) [""] req
        pure $ case res of
          Right (Left e) ->
            e === Right (HeaderParseError $ "could not parse: `" <> hval' <> "' (input does not start with a digit)")
          v -> counterexample ("Unexpected result: " <> show v) (property False)

prop_headerParseSuccess :: Property
prop_headerParseSuccess = property $ \(n :: Int) ->
  let req = wzero $ Request $ defaultRequest{requestHeaders = [("foo", fromString $ show n)]}
   in runIdentity $ do
        res <- runServerHandler (getTrait (Header :: RequiredHeader "foo" Int)) [""] req
        pure $ case res of
          Right (Right n') -> n === n'
          e -> counterexample ("Unexpected result: " <> show e) (property False)

-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Header" $allProperties
