module Main where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>), sconcat)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (fromStrict, toStrict)
import Flow
import Shikensu (Definition(..), Dictionary)
import Shikensu.Contrib (permalink, rename, renderContent)
import Text.Mustache as Mustache

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Text as Text (empty, pack)
import qualified Shikensu
import qualified Shikensu.Contrib.IO as Shikensu (read, write)


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO Dictionary
main =
    do
        partials            <- gatherMustachePartials

        -- Sequences
        pagesDictionary     <- pages partials

        -- Write to disk
        [ pagesDictionary ]
            |> List.concat
            |> Shikensu.write "."



-- ⚗️


listAndRead :: String -> String -> IO Dictionary
listAndRead pattern path =
    Shikensu.listRelative [ pattern ] path >>= Shikensu.read



-- Sequences ~ Pages (IO)
--
-- Take all the (mustache) pages
-- and render them.


pages :: Mustache.Template -> IO Dictionary
pages partials =
    let
        text =
            Text.pack .> Aeson.toJSON

        templateContext =
            Aeson.object
                [ ( "title", text "Shikensu" )
                ]

        flow dict =
            dict
                |> rename "Template.mustache" "index.html"
                |> renderContent (mustacheRenderer partials templateContext)
    in
        "./docs"
            |> listAndRead "*.mustache"
            |> fmap flow



-- Mustache (IO)
--
-- Here we take all the Mustache partials,
-- compile them and then put them together.


gatherMustachePartials :: IO Mustache.Template
gatherMustachePartials =
    "./docs/partials"
        |> listAndRead "**/*.mustache"
        |> fmap (fmap makeMustacheTemplate)
        |> fmap (foldl1 (<>))



-- Mustache (Pure)


{-| Transform a `Definition` into a Mustache `Template`.
-}
makeMustacheTemplate :: Definition -> Mustache.Template
makeMustacheTemplate def =
    let
        templateName =
            basename .> Text.pack .> PName
    in
        def
            |> content
            |> fmap decodeUtf8
            |> fromMaybe Text.empty
            |> compileMustacheText (templateName def)
            |> either (show .> error) id


{-| Shikensu renderer for Mustache templates.
-}
mustacheRenderer :: Mustache.Template -> Aeson.Value -> Definition -> Maybe ByteString
mustacheRenderer partials templateContext def =
    let
        injectPartials template =
            (<>) template partials

        render =
            flip renderMustache
    in
        def
            |> makeMustacheTemplate
            |> injectPartials
            |> render templateContext
            |> toStrict
            |> encodeUtf8
            |> Just
