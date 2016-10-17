module Shikensu.Contrib where

import Data.Maybe (fromMaybe)
import Flow
import Shikensu (absolutePath, forkDefinition, localPath)
import Shikensu.Types
import Shikensu.Utilities ((<&>), cleanPath, rmap)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, combine, joinPath, takeDirectory)

import qualified Data.List as List
import qualified Data.Map.Lazy as Map (union)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text (readFile, writeFile)
import qualified Shikensu (io, mapIO, mapPure, pure)


{-| Clone
-}
clone :: FilePath -> FilePath -> IO Dictionary -> IO Dictionary
clone existingPath newPath =
  let
    fn = \dict ->
      let
        makeNew = \def acc ->
          if (localPath def) == existingPath
            then acc ++ [forkDefinition newPath def]
            else acc
      in
        dict ++ (foldr makeNew [] dict)
  in
    Shikensu.pure fn



{-| Insert metadata
-}
insertMetadata :: Metadata -> IO Dictionary -> IO Dictionary
insertMetadata a = Shikensu.mapPure (insertMetadataDef a)


insertMetadataDef :: Metadata -> Definition -> Definition
insertMetadataDef given def = def { metadata = Map.union given (metadata def) }



{-| Permalink
-}
permalink :: String -> IO Dictionary -> IO Dictionary
permalink a = Shikensu.mapPure (permalinkDef a)


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



{-| Read
-}
read :: IO Dictionary -> IO Dictionary
read = Shikensu.mapIO (readDef)


readDef :: Definition -> IO Definition
readDef def =
  fmap
    (\c -> def { content = Just (Text.unpack c) })
    (Text.readFile $ absolutePath def)



{-| Rename
-}
rename :: FilePath -> FilePath -> IO Dictionary -> IO Dictionary
rename a b = Shikensu.mapPure (renameDef a b)


renameDef :: FilePath -> FilePath -> Definition -> Definition
renameDef oldPath newPath def =
  if (localPath def) == oldPath
    then forkDefinition newPath def
    else def



{-| Rename extension
-}
renameExt :: String -> String -> IO Dictionary -> IO Dictionary
renameExt a b = Shikensu.mapPure (renameExtDef a b)


renameExtDef :: String -> String -> Definition -> Definition
renameExtDef oldExtname newExtname def =
  if (extname def) == oldExtname
    then def { extname = newExtname }
    else def



{-| Write
-}
write :: FilePath -> IO Dictionary -> IO Dictionary
write a = Shikensu.mapIO (writeDef a)


writeDef :: FilePath -> Definition -> IO Definition
writeDef dest def =
  let
    path = joinPath [rootPath def, dest, localPath def]
    cont = fromMaybe "" (content def)
  in
    createDirectoryIfMissing True (takeDirectory path)
    >> Text.writeFile path (Text.pack cont)
    >> return def
