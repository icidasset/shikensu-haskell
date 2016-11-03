module Test.Contrib (contribTests) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Flow
import System.FilePath (joinPath)
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import qualified Data.List as List (head, reverse)
import qualified Data.Text as Text (intercalate, pack, unpack)
import qualified Data.Text.IO as Text (readFile)
import qualified Shikensu
import qualified Shikensu.Contrib as Contrib
import qualified Shikensu.Contrib.IO as Contrib.IO
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
    dictionary = fmap (Contrib.clone "example.md" "cloned.md") example_md
    definition = fmap (List.head . List.reverse) dictionary
  in
    testCase "Should `clone`"
      $ definition `rmap` Shikensu.localPath >>= assertEq "cloned.md"



testMetadata :: TestTree
testMetadata =
  let
    keyA        = Text.pack "title"
    valueA      = Aeson.String (Text.pack "Hello world!")

    keyB        = Text.pack "hello"
    valueB      = Aeson.String (Text.pack "Guardian.")

    keyC        = Text.pack "removed"
    valueC      = Aeson.String (Text.pack "Me.")

    -- 1. Insert C
    -- 2. Replace with A
    -- 3. Insert B
    dictionary = example_md
      <&> ( Contrib.insertMetadata (HashMap.fromList [ (keyC, valueC) ])
         .> Contrib.replaceMetadata (HashMap.fromList [ (keyA, valueA) ])
         .> Contrib.insertMetadata (HashMap.fromList [ (keyB, valueB) ])
      )

    definition  = fmap (List.head . List.reverse) dictionary

    lookupTitle = \def -> HashMap.lookup keyA (Shikensu.metadata def)
    lookupHello = \def -> HashMap.lookup keyB (Shikensu.metadata def)
    lookupRemoved = \def -> HashMap.lookup keyC (Shikensu.metadata def)
  in
    testGroup
      "Metadata"
      [ testCase "Should no longer have `removed` key"
        $ definition `rmap` lookupRemoved >>= assertEq Nothing

      , testCase "Should have `hello` key"
        $ definition `rmap` lookupHello >>= assertEq (Just valueB)

      , testCase "Should have `title` key"
        $ definition `rmap` lookupTitle >>= assertEq (Just valueA)
      ]



testPermalink :: TestTree
testPermalink =
  let
    dictionary = fmap (Contrib.permalink "index") example_md
    definition = fmap (List.head) dictionary
  in
    testCase "Should `permalink`"
      $ definition `rmap` Shikensu.localPath >>= assertEq "example/index.md"



testRead :: TestTree
testRead =
  let
    dictionary = example_md >>= Contrib.IO.read
    definition = fmap List.head dictionary
  in
    testCase "Should `read`"
      $ definition `rmap` Shikensu.content >>= assertEq (Just (Text.pack "# Example\n"))



testRename :: TestTree
testRename =
  let
    dictionary = fmap (Contrib.rename "example.md" "renamed.md") example_md
    definition = fmap (List.head) dictionary
  in
    testCase "Should `rename`"
      $ definition `rmap` Shikensu.localPath >>= assertEq "renamed.md"



testRenameExt :: TestTree
testRenameExt =
  let
    dictionary = fmap (Contrib.renameExt ".md" ".html") example_md
    definition = fmap (List.head) dictionary
  in
    testCase "Should `renameExt`"
      $ definition `rmap` Shikensu.extname >>= assertEq ".html"



testRenderContent :: TestTree
testRenderContent =
  let
    dictionary = fmap (Contrib.renderContent renderer) (example_md >>= Contrib.IO.read)
    definition = fmap (List.head) dictionary
    expectedResult = Just (Text.pack "<html># Example\n</html>")
  in
    testCase "Should `renderContent`"
      $ definition `rmap` Shikensu.content >>= assertEq expectedResult



testWrite :: TestTree
testWrite =
  let
    destination = "tests/build/"
    dictionary = list "tests/**/example.md" >>= Contrib.IO.read >>= Contrib.IO.write destination
    definition = fmap List.head dictionary
  in
    testCase "Should `write`"
      $ definition
          <&> Shikensu.rootDirname
          >>= \r -> Text.readFile (joinPath [r, destination, "fixtures/example.md"])
          >>= \c -> assertEq "# Example\n" (Text.unpack c)
