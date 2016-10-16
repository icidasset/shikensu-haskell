module Shikensu.Contrib where

import Data.Maybe (fromMaybe)
import Flow
import Shikensu (absolutePath, forkDefinition, localPath)
import Shikensu.Types
import Shikensu.Utilities ((<&>), rmap)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, joinPath, takeDirectory)

import qualified Data.List as List
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text (readFile, writeFile)
import qualified Shikensu (io, mapIO, mapPure, pure)


{-| Read
-}
read :: IO Dictionary -> IO Dictionary
read =
  let
    fn = \def ->
      fmap
        (\c -> def { content = Just (Text.unpack c) })
        (Text.readFile $ absolutePath def)
  in
    Shikensu.mapIO fn



{-| Rename
-}
rename :: FilePath -> FilePath -> IO Dictionary -> IO Dictionary
rename oldPath newPath =
  let
    fn = \def ->
      if (localPath def) == oldPath
        then forkDefinition newPath def
        else def
  in
    Shikensu.mapPure fn



{-| Write
-}
write :: FilePath -> IO Dictionary -> IO Dictionary
write dest =
  let
    fn = \def ->
      let
        path = joinPath [rootPath def, dest, localPath def]
        cont = fromMaybe "" $ content def
      in
        createDirectoryIfMissing True (takeDirectory path)
        >> Text.writeFile path (Text.pack cont)
        >> return def
  in
    Shikensu.mapIO fn
