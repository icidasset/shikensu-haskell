module Test.Utilities (utilityTests) where

import Flow
import Shikensu.Types
import Shikensu.Utilities as Utils
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.HashMap.Strict as HashMap (singleton)
import qualified Data.List as List (head)
import qualified Data.Tuple as Tuple (fst)
import qualified Shikensu
import qualified Shikensu.Contrib as Contrib


utilityTests :: TestTree
utilityTests = testGroup
    "Utility tests"
    [testSequencing, testThunder]




-- Tests


testSequencing :: TestTree
testSequencing =
    let
        result =
            Utils.lsequence
                [ ( "a", rootPath >>= Shikensu.listF ["tests/fixtures/example.md"] )
                , ( "b", rootPath >>= Shikensu.listF ["tests/fixtures/example.md"] )
                ]
    in
        testCase "Test lsequence"
        $ (List.head .> Tuple.fst) <$> result >>= assertEq "a"


testThunder :: TestTree
testThunder =
    let
        flow =
            Contrib.insertMetadata (HashMap.singleton "a" "Hi!")
            .> return

        dictionary =
            rootPath
                >>= Shikensu.listF ["tests/fixtures/example.md"]
                >>= flow

        def =
            fmap List.head dictionary

        resultExisting =
            fmap (\d -> metadata d ⚡ "a" :: Maybe String) def

        resultNonExisting =
            fmap (\d -> metadata d ⚡ "b" :: Maybe String) def
    in
        testGroup
            "Test (⚡)"
            [ testCase "Existing"
            $ resultExisting >>= assertEq (Just "Hi!")

            , testCase "Non-existing"
            $ resultNonExisting >>= assertEq (Nothing)
            ]
