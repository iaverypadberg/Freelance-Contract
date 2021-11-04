module Main
    ( main
    ) where

import qualified PropertyTests.Model
import qualified UnitTests.BasicTests
import qualified UnitTests.EndingTests
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "freelance tests"
    [  UnitTests.EndingTests.tests
    ]
--UnitTests.BasicTests.tests