module Unit (
  unitTests,
) where

import Test.Tasty (TestTree, testGroup)

import qualified Unit.Trait.Header as Header
import qualified Unit.Trait.Path as Path

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ Header.tests
    , Path.tests
    ]
