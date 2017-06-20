module Main where

import System.Directory (canonicalizePath, removePathForcibly)
import System.FilePath (combine)
import Test.Contrib
import Test.Example
import Test.Shikensu
import Test.Tasty
import Test.Utilities


main :: IO ()
main = do
    root        <- canonicalizePath "./"
    _           <- removePathForcibly (combine root "tests/build")

    defaultMain tests


tests :: TestTree
tests =
    testGroup
        "Tests"
        [ shikensuTests
        , contribTests
        , exampleTests
        , utilityTests
        ]
