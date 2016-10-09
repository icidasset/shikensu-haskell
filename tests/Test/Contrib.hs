module Test.Contrib (contribTests) where

import Flow
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List as List (head)
import qualified Shikensu
import qualified Shikensu.Contrib.IO as Contrib


contribTests :: TestTree
contribTests = testGroup "Contrib tests" [testA]



-- Tests


testA :: TestTree
testA =
  let
    intl = rootPath >>= Shikensu.list ["tests/fixtures/example.md"]
    dict = intl |> Contrib.read
    hdef = fmap List.head dict
  in
    testCase "Should have the correct content"
      $ (fmap Shikensu.content hdef) >>= (\x -> x >>= assertEqual "" "# Example\n")
