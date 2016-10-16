module Test.Contrib (contribTests) where

import Shikensu.Utilities ((<&>), rmap)
import System.FilePath (joinPath)
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List as List (head, reverse)
import qualified Data.Text.Lazy as Text (unpack)
import qualified Data.Text.Lazy.IO as Text (readFile)
import qualified Shikensu
import qualified Shikensu.Contrib as Contrib
import qualified Shikensu.Types as Shikensu (content, rootPath)


contribTests :: TestTree
contribTests = testGroup "Contrib tests" [testClone, testRead, testRename, testWrite]



-- Tests


testClone :: TestTree
testClone =
  let
    list = rootPath >>= Shikensu.list ["tests/fixtures/example.md"]
    dictionary = Contrib.clone "example.md" "cloned.md" list
    definition = fmap (List.head . List.reverse) dictionary
  in
    testCase "Should `clone`"
      $ definition `rmap` Shikensu.localPath >>= assertEq "cloned.md"


testRead :: TestTree
testRead =
  let
    list = rootPath >>= Shikensu.list ["tests/fixtures/example.md"]
    dictionary = Contrib.read list
    definition = fmap List.head dictionary
  in
    testCase "Should `read`"
      $ definition `rmap` Shikensu.content >>= assertEq (Just "# Example\n")


testRename :: TestTree
testRename =
  let
    list = rootPath >>= Shikensu.list ["tests/fixtures/example.md"]
    dictionary = Contrib.rename "example.md" "renamed.md" list
    definition = fmap List.head dictionary
  in
    testCase "Should `rename`"
      $ definition `rmap` Shikensu.localPath >>= assertEq "renamed.md"


testWrite :: TestTree
testWrite =
  let
    list = Contrib.read (rootPath >>= Shikensu.list ["tests/**/example.md"])
    destination = "tests/build/"
    dictionary = Contrib.write destination list
    definition = fmap List.head dictionary
  in
    testCase "Should `write`"
      $ definition
          <&> Shikensu.rootPath
          >>= \r -> Text.readFile (joinPath [r, destination, "fixtures/example.md"])
          >>= \c -> assertEq "# Example\n" (Text.unpack c)
