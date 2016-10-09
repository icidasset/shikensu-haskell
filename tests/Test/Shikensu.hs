module Test.Shikensu (shikensuTests) where


import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List as List (head)
import qualified Shikensu


shikensuTests :: TestTree
shikensuTests = testGroup "Shikensu tests" [testRegular, testDot, testWithoutWd]



-- Tests


testRegular :: TestTree
testRegular =
  let
    pattern = "tests/**/*.md"
    dictionary = fmap sort $ rootPath >>= Shikensu.list [pattern]
    definition = fmap List.head dictionary
    localPath = "fixtures/example.md"
  in
    testGroup "Test regular"
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
        $ assertDef definition Shikensu.workingDirectory "tests"

      ]


testDot :: TestTree
testDot =
  let
    pattern = "./tests/**/*.md"
    dictionary = fmap sort $ rootPath >>= Shikensu.list [pattern]
    definition = fmap List.head dictionary
    localPath = "fixtures/example.md"
  in
    testGroup "Test dot"
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
        $ assertDef definition Shikensu.workingDirectory "tests"

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
        $ assertDef definition Shikensu.workingDirectory ""

      ]
