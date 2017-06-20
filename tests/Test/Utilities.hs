module Test.Utilities
    ( utilityTests
    ) where

import Flow
import Shikensu
import Shikensu.Utilities as Utils
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.HashMap.Strict as HashMap (singleton)
import qualified Data.List as List (head)
import qualified Data.Tuple as Tuple (fst)
import qualified Shikensu.Contrib as Contrib


utilityTests :: TestTree
utilityTests = testGroup
    "Utility tests"
    [ testSequencing, testMetadataAccessors ]



-- Tests


testSequencing :: TestTree
testSequencing =
    let
        result =
            Utils.lsequence
                [ ( "a", Shikensu.listRelative ["tests/fixtures/example.md"] "." )
                , ( "b", Shikensu.listRelative ["tests/fixtures/example.md"] "." )
                ]
    in
        testCase "Test lsequence"
        $ fmap (List.head .> Tuple.fst) result >>= assertEq "a"


testMetadataAccessors :: TestTree
testMetadataAccessors =
    let
        definition =
            "."
                |> define "tests/fixtures/example.md"
                |> fmap (Contrib.insertMetadataDef $ HashMap.singleton "a" "Hi!")

        resultExisting =
            fmap (\d -> metadata d ~> "a" :: Maybe String) definition

        resultNonExisting =
            fmap (\d -> metadata d ~> "b" :: Maybe String) definition
    in
        testGroup
            "Test (~>)"
            [ testCase "Existing"
            $ assertEqm resultExisting (Just "Hi!")

            , testCase "Non-existing"
            $ assertEqm resultNonExisting Nothing
            ]
