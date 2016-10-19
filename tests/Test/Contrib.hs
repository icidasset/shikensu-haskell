module Test.Contrib (contribTests) where

import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Flow
import Shikensu.Utilities ((<&>), rmap)
import System.FilePath (joinPath)
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List as List (head, reverse)
import qualified Data.Map.Lazy as Map (fromList, lookup)
import qualified Data.Text.Lazy as Text (intercalate, pack, unpack)
import qualified Data.Text.Lazy.IO as Text (readFile)
import qualified Shikensu
import qualified Shikensu.Contrib as Contrib
import qualified Shikensu.Types as Shikensu


contribTests :: TestTree
contribTests = testGroup
  "Contrib tests"
  [ testClone
  , testMetadata
  , testPermalink
  , testRead
  , testRename
  , testRenameExt
  , testRenderContent
  , testWrite
  ]




-- Test data


list :: Shikensu.Pattern -> IO Shikensu.Dictionary
list pattern = rootPath >>= Shikensu.list [pattern]


example_md :: IO Shikensu.Dictionary
example_md = list "tests/fixtures/example.md"


renderer :: Shikensu.Definition -> Maybe Text
renderer def =
  let
    emptyText = Text.pack ""
  in
    Text.intercalate
      emptyText
      [ Text.pack "<html>"
      , fromMaybe emptyText (Shikensu.content def)
      , Text.pack "</html>"
      ]
      |> Just




-- Tests


testClone :: TestTree
testClone =
  let
    dictionary = Contrib.clone "example.md" "cloned.md" example_md
    definition = fmap (List.head . List.reverse) dictionary
  in
    testCase "Should `clone`"
      $ definition `rmap` Shikensu.localPath >>= assertEq "cloned.md"



testMetadata :: TestTree
testMetadata =
  let
    testData = [
        ("title", "Hello world!")
      , ("order", "1")
      ]

    dictionary = Contrib.insertMetadata (Map.fromList testData) example_md
    definition = fmap (List.head . List.reverse) dictionary

    lookupTitle = \def -> Map.lookup "title" (Shikensu.metadata def)
  in
    testCase "Should `metadata`"
      $ definition `rmap` lookupTitle >>= assertEq (Just "Hello world!")



testPermalink :: TestTree
testPermalink =
  let
    dictionary = Contrib.permalink "index" example_md
    definition = fmap List.head dictionary
  in
    testCase "Should `permalink`"
      $ definition `rmap` Shikensu.localPath >>= assertEq "example/index.md"



testRead :: TestTree
testRead =
  let
    dictionary = Contrib.read example_md
    definition = fmap List.head dictionary
  in
    testCase "Should `read`"
      $ definition `rmap` Shikensu.content >>= assertEq (Just (Text.pack "# Example\n"))



testRename :: TestTree
testRename =
  let
    dictionary = Contrib.rename "example.md" "renamed.md" example_md
    definition = fmap List.head dictionary
  in
    testCase "Should `rename`"
      $ definition `rmap` Shikensu.localPath >>= assertEq "renamed.md"



testRenameExt :: TestTree
testRenameExt =
  let
    dictionary = Contrib.renameExt ".md" ".html" example_md
    definition = fmap List.head dictionary
  in
    testCase "Should `renameExt`"
      $ definition `rmap` Shikensu.extname >>= assertEq ".html"



testRenderContent :: TestTree
testRenderContent =
  let
    dictionary = Contrib.renderContent renderer (Contrib.read example_md)
    definition = fmap List.head dictionary
    expectedResult = Just (Text.pack "<html># Example\n</html>")
  in
    testCase "Should `renderContent`"
      $ definition `rmap` Shikensu.content >>= assertEq expectedResult



testWrite :: TestTree
testWrite =
  let
    destination = "tests/build/"
    dictionary = Contrib.write destination $ (Contrib.read . list) "tests/**/example.md"
    definition = fmap List.head dictionary
  in
    testCase "Should `write`"
      $ definition
          <&> Shikensu.rootPath
          >>= \r -> Text.readFile (joinPath [r, destination, "fixtures/example.md"])
          >>= \c -> assertEq "# Example\n" (Text.unpack c)
