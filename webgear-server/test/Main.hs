module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Properties (propertyTests)
import Unit (unitTests)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests = testGroup "Tests" [unitTests, propertyTests, systemTests]

systemTests :: TestTree
systemTests = testGroup "System Tests" []
