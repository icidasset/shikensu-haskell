module Test.Shikensu
    ( shikensuTests
    ) where

import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List as List (head)
import qualified Shikensu


shikensuTests :: TestTree
shikensuTests = testGroup
    "Shikensu tests"
    [ testRegular
    , testDot
    , testWithoutWd
    , testRootFile
    ]



-- Tests


testRegular :: TestTree
testRegular =
    let
        definition = define "tests/**/*.md" "."
    in
        testGroup "Test regular"
            [ testCase "Should have the correct basename"
            $ assertDef definition Shikensu.basename "example"

            , testCase "Should have the correct dirname"
            $ assertDef definition Shikensu.dirname "fixtures"

            , testCase "Should have the correct extname"
            $ assertDef definition Shikensu.extname ".md"

            , testCase "Should have the correct pattern"
            $ assertDef definition Shikensu.pattern "tests/**/*.md"

            , testCase "Should have the correct workingDirname"
            $ assertDef definition Shikensu.workingDirname "tests"

            ]


testDot :: TestTree
testDot =
    let
        definition = define "./tests/**/*.md" "."
    in
        testGroup "Test dot"
            [ testCase "Should have the correct basename"
            $ assertDef definition Shikensu.basename "example"

            , testCase "Should have the correct dirname"
            $ assertDef definition Shikensu.dirname "fixtures"

            , testCase "Should have the correct extname"
            $ assertDef definition Shikensu.extname ".md"

            , testCase "Should have the correct pattern"
            $ assertDef definition Shikensu.pattern "./tests/**/*.md"

            , testCase "Should have the correct workingDirname"
            $ assertDef definition Shikensu.workingDirname "tests"

            ]


testWithoutWd :: TestTree
testWithoutWd =
    let
        definition = define "**/*.md" "./tests"
    in
        testGroup "Test without workingDirname"
            [ testCase "Should have the correct basename"
            $ assertDef definition Shikensu.basename "example"

            , testCase "Should have the correct dirname"
            $ assertDef definition Shikensu.dirname "fixtures"

            , testCase "Should have the correct extname"
            $ assertDef definition Shikensu.extname ".md"

            , testCase "Should have the correct pattern"
            $ assertDef definition Shikensu.pattern "**/*.md"

            , testCase "Should have the correct workingDirname"
            $ assertDef definition Shikensu.workingDirname ""

            ]


testRootFile :: TestTree
testRootFile =
    let
        definition = define "*.md" "."
    in
        testGroup "Test file in root path"
            [ testCase "Should have the correct basename"
            $ assertDef definition Shikensu.basename "CHANGELOG"

            , testCase "Should have the correct dirname"
            $ assertDef definition Shikensu.dirname ""

            , testCase "Should have the correct extname"
            $ assertDef definition Shikensu.extname ".md"

            , testCase "Should have the correct pattern"
            $ assertDef definition Shikensu.pattern "*.md"

            , testCase "Should have the correct workingDirname"
            $ assertDef definition Shikensu.workingDirname ""

            ]
