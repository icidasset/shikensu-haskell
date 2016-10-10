module Test.Contrib (contribTests) where

import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List as List (head)
import qualified Shikensu
import qualified Shikensu.Contrib.IO as Contrib
import qualified Shikensu.Types as Shikensu (content)


contribTests :: TestTree
contribTests = testGroup "Contrib tests" [testRead]



-- Tests


testRead :: TestTree
testRead =
  let
    list = rootPath >>= Shikensu.list ["tests/fixtures/example.md"]
    dictionary = Contrib.read list
    definition = fmap List.head dictionary
  in
    testCase "Should have the correct content"
      $ definition >>= Shikensu.content >>= assertEq "# Example\n"
