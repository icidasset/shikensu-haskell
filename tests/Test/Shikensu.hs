module Test.Shikensu (shikensuTests) where


import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List as List (head)
import qualified Shikensu
import qualified Shikensu.Types as Shikensu


shikensuTests :: TestTree
shikensuTests = testGroup
  "Shikensu tests"
  [testRegular, testDot, testWithoutWd, testRootFile]



-- Tests


testRegular :: TestTree
testRegular =
  let
    pattern = "tests/**/*.md"
    dictionary = fmap sort $ rootPath >>= Shikensu.list [pattern]
    definition = fmap List.head dictionary
    localPath = "tests/fixtures/example.md"
  in
    testGroup "Test regular"
      [ testCase "Should have the correct basename"
        $ definition `rmap` Shikensu.basename >>= assertEq "example"

      , testCase "Should have the correct dirname"
        $ definition `rmap` Shikensu.dirname >>= assertEq "fixtures"

      , testCase "Should have the correct extname"
        $ definition `rmap` Shikensu.extname >>= assertEq ".md"

      , testCase "Should have the correct localPath"
        $ definition `rmap` Shikensu.localPath >>= assertEq localPath

      , testCase "Should have the correct pattern"
        $ definition `rmap` Shikensu.pattern >>= assertEq pattern

      , testCase "Should have the correct workingDirectory"
        $ definition `rmap` Shikensu.workingDirectory >>= assertEq "tests"

      ]


testDot :: TestTree
testDot =
  let
    pattern = "./tests/**/*.md"
    dictionary = fmap sort $ rootPath >>= Shikensu.list [pattern]
    definition = fmap List.head dictionary
    localPath = "tests/fixtures/example.md"
  in
    testGroup "Test dot"
      [ testCase "Should have the correct basename"
        $ definition `rmap` Shikensu.basename >>= assertEq "example"

      , testCase "Should have the correct dirname"
        $ definition `rmap` Shikensu.dirname >>= assertEq "fixtures"

      , testCase "Should have the correct extname"
        $ definition `rmap` Shikensu.extname >>= assertEq ".md"

      , testCase "Should have the correct localPath"
        $ definition `rmap` Shikensu.localPath >>= assertEq localPath

      , testCase "Should have the correct pattern"
        $ definition `rmap` Shikensu.pattern >>= assertEq pattern

      , testCase "Should have the correct workingDirectory"
        $ definition `rmap` Shikensu.workingDirectory >>= assertEq "tests"

      ]


testWithoutWd :: TestTree
testWithoutWd =
  let
    pattern = "**/*.md"
    dictionary = fmap sort $ testsPath >>= Shikensu.list [pattern]
    definition = fmap List.head dictionary
    localPath = "fixtures/example.md"
  in
    testGroup "Test without workingDirectory"
      [ testCase "Should have the correct basename"
        $ definition `rmap` Shikensu.basename >>= assertEq "example"

      , testCase "Should have the correct dirname"
        $ definition `rmap` Shikensu.dirname >>= assertEq "fixtures"

      , testCase "Should have the correct extname"
        $ definition `rmap` Shikensu.extname >>= assertEq ".md"

      , testCase "Should have the correct localPath"
        $ definition `rmap` Shikensu.localPath >>= assertEq localPath

      , testCase "Should have the correct pattern"
        $ definition `rmap` Shikensu.pattern >>= assertEq pattern

      , testCase "Should have the correct workingDirectory"
        $ definition `rmap` Shikensu.workingDirectory >>= assertEq ""

      ]


testRootFile :: TestTree
testRootFile =
  let
    pattern = "*.md"
    dictionary = fmap sort $ rootPath >>= Shikensu.list [pattern]
    definition = fmap List.head dictionary
    localPath = "README.md"
  in
    testGroup "Test file in root path"
      [ testCase "Should have the correct basename"
        $ definition `rmap` Shikensu.basename >>= assertEq "README"

      , testCase "Should have the correct dirname"
        $ definition `rmap` Shikensu.dirname >>= assertEq ""

      , testCase "Should have the correct extname"
        $ definition `rmap` Shikensu.extname >>= assertEq ".md"

      , testCase "Should have the correct localPath"
        $ definition `rmap` Shikensu.localPath >>= assertEq localPath

      , testCase "Should have the correct pattern"
        $ definition `rmap` Shikensu.pattern >>= assertEq pattern

      , testCase "Should have the correct workingDirectory"
        $ definition `rmap` Shikensu.workingDirectory >>= assertEq ""

      ]
