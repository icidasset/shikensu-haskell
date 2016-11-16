module Test.Example (exampleTests) where

import Data.Text (Text)
import System.Directory (canonicalizePath)
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)
import qualified Shikensu

import Data.ByteString (ByteString)
import Flow
import Prelude hiding (read)
import Shikensu.Types
import Shikensu.Contrib
import Shikensu.Contrib.IO (read, write)


exampleTests :: TestTree
exampleTests =
  let
    path = canonicalizePath "./tests"
    dict = path >>= dictionary_io
    test = path >>= \p ->
      read [Shikensu.makeDefinition p "fixtures/*.md" "fixtures/example.md"]
      >>= flow
  in
    testCase "Example test"
      $ dict >>= \d -> test >>= assertEq d



-- Setup


dictionary_io :: String -> IO Dictionary
dictionary_io absolutePathToCwd =
  Shikensu.list ["fixtures/*.md"] absolutePathToCwd
    >>= read
    >>= flow
    >>= write "./build"


flow :: Dictionary -> IO Dictionary
flow =
     renameExt ".md" ".html"
  .> permalink "index"
  .> clone "index.html" "200.html"
  .> copyPropsToMetadata
  .> renderContent markdownRenderer
  .> return


markdownRenderer :: Definition -> Maybe ByteString
markdownRenderer def =
  content def
    |> fmap Text.decodeUtf8
    |> fmap renderMarkdown
    |> fmap Text.encodeUtf8


renderMarkdown :: Text -> Text
renderMarkdown text = text
