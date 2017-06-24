module Test.Contrib
    ( contribTests
    ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Flow
import System.FilePath
import Test.Helpers
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified Data.ByteString as B (empty)
import qualified Data.ByteString.Char8 as BS (intercalate, pack)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import qualified Data.List as List (head, reverse)
import qualified Data.Text as Text (pack, unpack)
import qualified Data.Text.Encoding as Text (decodeUtf8)
import qualified Data.Text.IO as Text (readFile)
import qualified Shikensu
import qualified Shikensu.Contrib as Contrib
import qualified Shikensu.Contrib.IO as Contrib.IO


contribTests :: TestTree
contribTests = testGroup
    "Contrib tests"
    [ testClone
    , testExclude
    , testMetadata
    , testPermalink
    , testPrefixDirname
    , testRead
    , testRename
    , testRenameExt
    , testRenderContent
    , testWrite
    ]



-- Test data


list :: String -> IO Shikensu.Dictionary
list thePattern =
    Shikensu.listRelative [thePattern] "."


example_md :: IO Shikensu.Dictionary
example_md =
    list $ joinPath [ "tests", "fixtures", "example.md" ]


renderer :: Shikensu.Definition -> Maybe ByteString
renderer def =
    let
        openingTag = BS.pack "<html>"
        closingTag = BS.pack "</html>"
    in
        def
            |> Shikensu.content
            |> fmap (\c -> BS.intercalate B.empty [openingTag, c, closingTag])


upstairs :: String
upstairs =
    addTrailingPathSeparator ".."



-- Tests


testClone :: TestTree
testClone =
    let
        dictionary = fmap (Contrib.clone "example.md" "cloned.md") example_md
        definition = fmap (List.reverse .> List.head) dictionary
    in
        testCase "Should `clone`"
        $ assertDef definition Shikensu.localPath "cloned.md"



testExclude :: TestTree
testExclude =
    let
        dictionary = fmap (Contrib.exclude "example.md") example_md
        dictLength = fmap length dictionary
    in
        testCase "Should `exclude`"
        $ assertEqm dictLength 0



testMetadata :: TestTree
testMetadata =
    let
        keyA        = Text.pack "title"
        valueA      = Aeson.String (Text.pack "Hello world!")

        keyB        = Text.pack "hello"
        valueB      = Aeson.String (Text.pack "Guardian.")

        keyC        = Text.pack "removed"
        valueC      = Aeson.String (Text.pack "Me.")

        keyBase     = Text.pack "basename"
        valueBase   = Aeson.String (Text.pack "example")

        -- 1. Insert C
        -- 2. Replace with A
        -- 3. Insert B
        dictionary = fmap
            ( id
                .> Contrib.insertMetadata   (HashMap.fromList [ (keyC, valueC) ])
                .> Contrib.replaceMetadata  (HashMap.fromList [ (keyA, valueA) ])
                .> Contrib.copyPropsToMetadata
                .> Contrib.insertMetadata   (HashMap.fromList [ (keyB, valueB) ])
            )
            example_md

        definition          = fmap (List.reverse .> List.head) dictionary

        lookupTitle     def = HashMap.lookup keyA (Shikensu.metadata def)
        lookupHello     def = HashMap.lookup keyB (Shikensu.metadata def)
        lookupRemoved   def = HashMap.lookup keyC (Shikensu.metadata def)
        lookupBasename  def = HashMap.lookup keyBase (Shikensu.metadata def)
    in
        testGroup
            "Metadata"
            [ testCase "Should no longer have `removed` key"
            $ assertDef definition lookupRemoved Nothing

            , testCase "Should have `hello` key"
            $ assertDef definition lookupHello (Just valueB)

            , testCase "Should have `title` key"
            $ assertDef definition lookupTitle (Just valueA)

            , testCase "Should have `basename` key"
            $ assertDef definition lookupBasename (Just valueBase)
            ]



testPermalink :: TestTree
testPermalink =
    let
        dictionary = fmap (Contrib.permalink "index") example_md
        definition = fmap List.head dictionary
    in
        testGroup
            "Permalink"
            [ testCase "Should have the correct `localPath`"
            $ assertDef definition Shikensu.localPath ("example" </> "index.md")

            , testCase "Should have the correct `parentPath`"
            $ assertDef definition Shikensu.parentPath (Just upstairs)

            , testCase "Should have the correct `pathToRoot`"
            $ assertDef definition Shikensu.pathToRoot upstairs
            ]



testPrefixDirname :: TestTree
testPrefixDirname =
    let
        thePrefix  = addTrailingPathSeparator "prefix"
        thePattern = "tests" </> "**" </> "example.md"

        dictionary = fmap (Contrib.prefixDirname thePrefix) (list thePattern)
        definition = fmap List.head dictionary
    in
        testGroup
            "PrefixDirname"
            [ testCase "Should have the correct `dirname`"
            $ assertDef definition Shikensu.dirname ("prefix" </> "fixtures")

            , testCase "Should have the correct `parentPath`"
            $ assertDef definition Shikensu.parentPath (Just upstairs)

            , testCase "Should have the correct `pathToRoot`"
            $ assertDef definition Shikensu.pathToRoot (upstairs <> upstairs)
            ]



testRead :: TestTree
testRead =
    let
        theResult  = Just (Text.pack "# Example\n")
        definition = fmap List.head (example_md >>= Contrib.IO.read)
    in
        testCase "Should `read`"
        $ assertDef definition (Shikensu.content .> fmap Text.decodeUtf8) theResult



testRename :: TestTree
testRename =
    let
        dictionary = fmap (Contrib.rename "example.md" "renamed.md") example_md
        definition = fmap List.head dictionary
    in
        testCase "Should `rename`"
        $ assertDef definition Shikensu.localPath "renamed.md"



testRenameExt :: TestTree
testRenameExt =
    let
        dictionary = fmap (Contrib.renameExt ".md" ".html") example_md
        definition = fmap List.head dictionary
    in
        testCase "Should `renameExt`"
        $ assertDef definition Shikensu.extname ".html"



testRenderContent :: TestTree
testRenderContent =
    let
        theResult  = Just (Text.pack "<html># Example\n</html>")
        dictionary = fmap (Contrib.renderContent renderer) (example_md >>= Contrib.IO.read)
        definition = fmap List.head dictionary
    in
        testCase "Should `renderContent`"
        $ assertDef definition (Shikensu.content .> fmap Text.decodeUtf8) theResult



testWrite :: TestTree
testWrite =
    let
        destination = "tests" </> "build" <> [ pathSeparator ]
        thePattern  = "tests" </> "**" </> "example.md"

        dictionary  = list thePattern
            >>= Contrib.IO.read
            >>= Contrib.IO.write destination

        definition = fmap List.head dictionary
    in
        testCase "Should `write`"
        $ fmap Shikensu.rootDirname definition
            >>= \r -> readFile (joinPath [ r, destination, "fixtures", "example.md" ])
            >>= \c -> assertEq "# Example\n" c
