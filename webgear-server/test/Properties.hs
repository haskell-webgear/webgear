module Properties (
  propertyTests,
) where

import Test.Tasty (TestTree, testGroup)

import qualified Properties.Trait.Auth.Basic as Basic
import qualified Properties.Trait.Body as Body
import qualified Properties.Trait.Header as Header
import qualified Properties.Trait.Method as Method
import qualified Properties.Trait.Path as Path
import qualified Properties.Trait.QueryParam as QueryParam

propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Tests"
    [ Method.tests
    , Path.tests
    , Header.tests
    , QueryParam.tests
    , Body.tests
    , Basic.tests
    ]
