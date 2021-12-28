{-| Dictionary functions.

-}
module Shikensu.Contrib
    ( clearMetadata
    , clearMetadataDef
    , clone
    , copyPropsToMetadata
    , copyPropsToMetadataDef
    , exclude
    , insertMetadata
    , insertMetadataDef
    , permalink
    , permalinkDef
    , prefixDirname
    , prefixDirnameDef
    , rename
    , renameDef
    , renameExt
    , renameExtDef
    , renderContent
    , renderContentDef
    , replaceMetadata
    , replaceMetadataDef
    , setContent
    , setContentDef
    , transformContent
    , transformContentDef
    ) where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Shikensu (forkDefinition)
import Shikensu.Internal.Types
import Shikensu.Internal.Utilities (compileParentPath, compilePathToRoot)
import Shikensu.Metadata (transposeToMetadata)
import System.FilePath (FilePath, combine)

import qualified Data.Aeson.KeyMap as KeyMap (empty, union)


{-| Clear metadata.

Replace the current hash map with an empty one.
-}
clearMetadata :: Dictionary -> Dictionary
clearMetadata =
    fmap clearMetadataDef


clearMetadataDef :: Definition -> Definition
clearMetadataDef def =
    def { metadata = KeyMap.empty }



{-| Clone.

For each definition that has the given `localPath` (1st argument),
make a clone with a new `localPath` (2nd argument),
and add that into dictionary just after the matching definition.

> clone "index.html" "200.html" dictionary
-}
clone :: FilePath -> FilePath -> Dictionary -> Dictionary
clone existingPath newPath dict =
    let
        makeNew = \def acc ->
            if localPath def == existingPath
               then acc <> [forkDefinition newPath def]
               else acc
    in
        dict <> foldr makeNew [] dict



{-| Copy definition properties into the metadata.

See the `toJSON` implementation for `Definition` in `Shikensu.Types`
to see what properties get put in here.
-}
copyPropsToMetadata :: Dictionary -> Dictionary
copyPropsToMetadata =
    fmap copyPropsToMetadataDef


copyPropsToMetadataDef :: Definition -> Definition
copyPropsToMetadataDef def =
    def
        { metadata = KeyMap.union (transposeToMetadata def) (metadata def) }



{-| Exclude.

Filter out the definitions that have the given `localPath`.
-}
exclude :: FilePath -> Dictionary -> Dictionary
exclude path =
    filter (\def -> localPath def /= path)



{-| Insert metadata.

Merge the current hash map with another one.
-}
insertMetadata :: Metadata -> Dictionary -> Dictionary
insertMetadata a =
    fmap (insertMetadataDef a)


insertMetadataDef :: Metadata -> Definition -> Definition
insertMetadataDef given def =
    def { metadata = KeyMap.union given (metadata def) }



{-| Permalink.

Append the basename to the dirname,
and change the basename to the given string.
It will NOT change definitions that already have the new basename.

> permalink "index" dictionary
-}
permalink :: String -> Dictionary -> Dictionary
permalink a =
    fmap (permalinkDef a)


permalinkDef :: String -> Definition -> Definition
permalinkDef newBasename def =
    if basename def /= newBasename then
       let
           newDirname = combine (dirname def) (basename def)
       in
           def
               { basename    = newBasename
               , dirname     = newDirname
               , parentPath  = compileParentPath newDirname
               , pathToRoot  = compilePathToRoot newDirname
               }
    else
        def



{-| Prefix dirname.

Prefix the dirname of each definition with a given string.
-}
prefixDirname :: String -> Dictionary -> Dictionary
prefixDirname prefix = fmap (prefixDirnameDef prefix)


prefixDirnameDef :: String -> Definition -> Definition
prefixDirnameDef prefix def =
    let
        newDirname = prefix <> dirname def
    in
        def
            { dirname     = newDirname
            , parentPath  = compileParentPath newDirname
            , pathToRoot  = compilePathToRoot newDirname
            }



{-| Rename.

Change the `localPath` of the definitions that match a given `localPath`.
For example, if you have a definition with the local path `a/b/example.html`:

> rename "a/b/example.html" "example/index.html" dictionary

See `Shikensu.localPath` for more info.
-}
rename :: FilePath -> FilePath -> Dictionary -> Dictionary
rename a b = fmap (renameDef a b)


renameDef :: FilePath -> FilePath -> Definition -> Definition
renameDef oldPath newPath def =
  if localPath def == oldPath
     then forkDefinition newPath def
     else def



{-| Rename extension.

Example:

> renameExt ".markdown" ".html" dictionary
> -- The definitions that had the extname ".markdown"
> -- now have the extname ".html"
-}
renameExt :: String -> String -> Dictionary -> Dictionary
renameExt a b =
    fmap (renameExtDef a b)


renameExtDef :: String -> String -> Definition -> Definition
renameExtDef oldExtname newExtname def =
    if extname def == oldExtname
        then def { extname = newExtname }
        else def



{-| Render content.

Replace the content property by providing a renderer.
A renderer is a function with the signature `Definition -> Maybe ByteString`.

You can use this to render templates, markdown, etc.
-}
renderContent :: (Definition -> Maybe ByteString) -> Dictionary -> Dictionary
renderContent a =
    fmap (renderContentDef a)


renderContentDef :: (Definition -> Maybe ByteString) -> Definition -> Definition
renderContentDef renderer def =
    def { content = renderer def }



{-| Replace metadata.

Replace the current hash map with another one.
-}
replaceMetadata :: Metadata -> Dictionary -> Dictionary
replaceMetadata a =
    fmap (replaceMetadataDef a)


replaceMetadataDef :: Metadata -> Definition -> Definition
replaceMetadataDef given def =
    def { metadata = given }


{-| Set content.

Set content directly.
-}
setContent :: ByteString -> Dictionary -> Dictionary
setContent content =
    fmap (setContentDef content)


setContentDef :: ByteString -> Definition -> Definition
setContentDef content def =
    def { content = Just content }


{-| Transform content.

Alias for `renderContent`.
-}
transformContent :: (Definition -> Maybe ByteString) -> Dictionary -> Dictionary
transformContent = renderContent


transformContentDef :: (Definition -> Maybe ByteString) -> Definition -> Definition
transformContentDef =
    renderContentDef
