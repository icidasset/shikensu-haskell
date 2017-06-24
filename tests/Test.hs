module Main where

import System.Directory (getCurrentDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Contrib
import Test.Example
import Test.Shikensu
import Test.Tasty
import Test.Utilities


main :: IO ()
main = do
    root        <- getCurrentDirectory
    _           <- removePathForcibly (root </> "tests" </> "build")

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
