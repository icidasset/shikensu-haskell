{-# LANGUAGE OverloadedStrings #-}
module Shikensu.Contrib
  ( clone
  , insertMetadata
  , insertMetadataDef
  , permalink
  , permalinkDef
  , rename
  , renameDef
  , renameExt
  , renameExtDef
  , renderContent
  , renderContentDef
  , replaceMetadata
  , replaceMetadataDef
  ) where

import Data.Text (Text)
import Flow
import Shikensu (forkDefinition)
import Shikensu.Types
import Shikensu.Utilities (cleanPath)
import System.FilePath (FilePath, combine)

import qualified Data.HashMap.Strict as HashMap (union)


{-| Clone
-}
clone :: FilePath -> FilePath -> Dictionary -> Dictionary
clone existingPath newPath dict =
  let
    makeNew = \def acc ->
      if (localPath def) == existingPath
        then acc ++ [forkDefinition newPath def]
        else acc
  in
    dict ++ (foldr makeNew [] dict)



{-| Insert metadata
-}
insertMetadata :: Metadata -> Dictionary -> Dictionary
insertMetadata a = fmap (insertMetadataDef a)


insertMetadataDef :: Metadata -> Definition -> Definition
insertMetadataDef given def = def { metadata = HashMap.union given (metadata def) }



{-| Permalink
-}
permalink :: String -> Dictionary -> Dictionary
permalink a = fmap (permalinkDef a)


permalinkDef :: String -> Definition -> Definition
permalinkDef newBasename def =
  if (basename def) /= newBasename
    then
      let
        newDirname = cleanPath $ combine (dirname def) (basename def)
      in
        def { basename = newBasename, dirname = newDirname }

    else
      def



{-| Rename
-}
rename :: FilePath -> FilePath -> Dictionary -> Dictionary
rename a b = fmap (renameDef a b)


renameDef :: FilePath -> FilePath -> Definition -> Definition
renameDef oldPath newPath def =
  if (localPath def) == oldPath
    then forkDefinition newPath def
    else def



{-| Rename extension
-}
renameExt :: String -> String -> Dictionary -> Dictionary
renameExt a b = fmap (renameExtDef a b)


renameExtDef :: String -> String -> Definition -> Definition
renameExtDef oldExtname newExtname def =
  if (extname def) == oldExtname
    then def { extname = newExtname }
    else def



{-| Render content
-}
renderContent :: (Definition -> Maybe Text) -> Dictionary -> Dictionary
renderContent a = fmap (renderContentDef a)


renderContentDef :: (Definition -> Maybe Text) -> Definition -> Definition
renderContentDef renderer def = def { content = renderer def }



{-| Replace metadata
-}
replaceMetadata :: Metadata -> Dictionary -> Dictionary
replaceMetadata a = fmap (replaceMetadataDef a)


replaceMetadataDef :: Metadata -> Definition -> Definition
replaceMetadataDef given def = def { metadata = given }
