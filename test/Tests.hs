module Main
    ( main
    ) where

import qualified PropertyTests.Model
import qualified UnitTests.BasicTests
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "freelance tests"
    [ UnitTests.BasicTests.tests
    -- , PropertyTests.Model.tests
    ]
