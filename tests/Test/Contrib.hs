module Test.Contrib (contribTests) where

import System.FilePath (joinPath)
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List as List (head)
import qualified Data.Text.Lazy as Text (unpack)
import qualified Data.Text.Lazy.IO as Text (readFile)
import qualified Shikensu
import qualified Shikensu.Contrib as Contrib
import qualified Shikensu.Types as Shikensu (content, rootPath)


contribTests :: TestTree
contribTests = testGroup "Contrib tests" [testRead, testWrite]



-- Tests


testRead :: TestTree
testRead =
  let
    list = rootPath >>= Shikensu.list ["tests/fixtures/example.md"]
    dictionary = Contrib.read list
    definition = fmap List.head dictionary
  in
    testCase "Should `read`"
      $ definition `rmap` Shikensu.content >>= assertEq (Just "# Example\n")


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
