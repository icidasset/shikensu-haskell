module Test.Example
    ( exampleTests
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Flow
import Prelude hiding (read)
import Shikensu (Definition(..), Dictionary(..), list, makeDefinition)
import Shikensu.Contrib (clone, copyPropsToMetadata, permalink, renameExt, renderContent)
import Shikensu.Contrib.IO (read, write)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>), joinPath)
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)


exampleTests :: TestTree
exampleTests =
    testCase "Example test" $ dictionaries >>= uncurry assertEq


thePattern :: String
thePattern =
    "fixtures" </> "*.md"


dictionaries :: IO (Dictionary, Dictionary)
dictionaries = do
    root            <- getCurrentDirectory

    let testsDir    = root </> "tests"
    dictA           <- dictionary_io testsDir

    let absolute    = testsDir </> "fixtures" </> "example.md"
    let dictB       = [ Shikensu.makeDefinition testsDir thePattern absolute ]

    dictB_Read      <- read dictB
    dictB_Final     <- flow dictB_Read

    return (dictA, dictB_Final)



-- Setup


dictionary_io :: String -> IO Dictionary
dictionary_io absolutePathToCwd =
    Shikensu.list [thePattern] absolutePathToCwd
        >>= read
        >>= flow
        >>= write "build"


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
renderMarkdown text =
    text
