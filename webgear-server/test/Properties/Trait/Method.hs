module Properties.Trait.Method (
  tests,
) where

import Data.Functor.Identity (runIdentity)
import Network.HTTP.Types (StdMethod (..), methodGet, renderStdMethod)
import Network.Wai (defaultRequest, requestMethod)
import Test.QuickCheck (
  Arbitrary (arbitrary),
  Property,
  allProperties,
  elements,
  property,
  (.&&.),
  (=/=),
  (===),
 )
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperties)
import WebGear.Core.Request (Request (..))
import WebGear.Core.Trait (getTrait, linkzero)
import WebGear.Core.Trait.Method (Method (..), MethodMismatch (..))
import WebGear.Server.Handler (runServerHandler)
import WebGear.Server.Trait.Method ()

newtype MethodWrapper = MethodWrapper StdMethod
  deriving stock (Show)

instance Arbitrary MethodWrapper where
  arbitrary = elements $ MethodWrapper <$> [minBound .. maxBound]

prop_methodMatch :: Property
prop_methodMatch = property $ \(MethodWrapper v) ->
  let req = linkzero $ Request $ defaultRequest{requestMethod = renderStdMethod v}
   in runIdentity $ do
        res <- runServerHandler (getTrait (Method GET mempty)) [""] req
        pure $ case res of
          Right (Right _) -> v === GET
          Right (Left e) -> expectedMethod e === methodGet .&&. actualMethod e =/= methodGet
          Left _ -> property False

-- Hack for TH splicing
return []

tests :: TestTree
tests = testProperties "Trait.Method" $allProperties
