import System.Directory (canonicalizePath)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List as List
import qualified Shikensu


{-| TODO

Write tests for multiple cases:

1. Pattern with "static" directory (e.g. "src/**/*.hs")
2. Pattern without "static" directory (e.g. "**/*.hs")
3. Pattern with leading dot-slash (e.g. "./src/**/*.hs")
4. ...

-}


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [unitTests]



-- Test data


rootPath :: IO FilePath
rootPath = canonicalizePath "./"



-- Helpers


assertDef :: (Show b, Eq b) => IO a -> (a -> b) -> b -> IO ()
assertDef def recFn value =
  fmap recFn def >>= assertEqual "" value


sort :: Shikensu.Dictionary -> Shikensu.Dictionary
sort = List.sortBy Shikensu.sortByAbsolutePath



-- Tests


unitTests :: TestTree
unitTests = testGroup "Unit tests" [testA]


testA :: TestTree
testA =
  let
    pattern = "test/**/*.md"
    dictionary = fmap sort $ rootPath >>= Shikensu.list [pattern]
    definition = fmap List.head dictionary
    localPath = "fixtures/example.md"
  in
    testGroup "Unit tests"
      [ testCase "Should have the correct basename"
        $ assertDef definition Shikensu.basename "example"

      , testCase "Should have the correct dirname"
        $ assertDef definition Shikensu.dirname "fixtures"

      , testCase "Should have the correct extname"
        $ assertDef definition Shikensu.extname ".md"

      , testCase "Should have the correct localPath"
        $ assertDef definition Shikensu.localPath localPath

      , testCase "Should have the correct pattern"
        $ assertDef definition Shikensu.pattern pattern

      , testCase "Should have the correct workingDirectory"
        $ assertDef definition Shikensu.workingDirectory "test"

      ]
