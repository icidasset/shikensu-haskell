module Test.Helpers
    ( assertDef
    , assertEq
    , assertEqm
    , define
    ) where

import Flow
import Shikensu
import Shikensu.Sorting (sortByAbsolutePath)
import Test.Tasty.HUnit (Assertion, assertEqual)

import qualified Data.List as List (head, sortBy)


-- Assertions


assertDef :: (Eq a, Show a) => IO Definition -> (Definition -> a) -> a -> Assertion
assertDef definition recordFn value =
    fmap recordFn definition >>= assertEq value


assertEq :: (Eq a, Show a) => a -> a -> Assertion
assertEq =
    assertEqual ""


assertEqm :: (Eq a, Show a) => IO a -> a -> Assertion
assertEqm a b =
    a >>= assertEqual "" b



-- Shortcuts


define :: String -> String -> IO Definition
define thePattern root =
    root
        |> Shikensu.listRelative [ thePattern ]
        |> fmap (List.sortBy sortByAbsolutePath)
        |> fmap List.head
