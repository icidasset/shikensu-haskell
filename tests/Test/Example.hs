module Test.Example
    ( exampleTests
    ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Flow
import Prelude hiding (read)
import Shikensu (Definition(..), Dictionary(..), list, makeDefinition)
import Shikensu.Contrib (clone, copyPropsToMetadata, permalink, renameExt, renderContent)
import Shikensu.Contrib.IO (read, write)
import System.Directory (canonicalizePath)
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text.Encoding as Text (decodeUtf8, encodeUtf8)


exampleTests :: TestTree
exampleTests =
    testCase "Example test" $ dictionaries >>= uncurry assertEq


dictionaries :: IO (Dictionary, Dictionary)
dictionaries = do
    root            <- canonicalizePath "./tests"
    dictA           <- dictionary_io root

    let absolute    = root <> "/fixtures/example.md"
    let dictB       = [Shikensu.makeDefinition root "fixtures/*.md" absolute]

    dictB_Read      <- read dictB
    dictB_Final     <- flow dictB_Read

    return (dictA, dictB_Final)



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
renderMarkdown text =
    text
