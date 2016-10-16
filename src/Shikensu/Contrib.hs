module Shikensu.Contrib where

import Data.Maybe (fromMaybe)
import Flow
import Shikensu (absolutePath, forkDefinition, localPath)
import Shikensu.Types
import Shikensu.Utilities ((<&>), rmap)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, combine, takeDirectory)

import qualified Data.List as List
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text (readFile, writeFile)


{-| Read
-}
read :: IO Dictionary -> IO Dictionary
read = (=<<) $ List.map (readDef) .> sequence


readDef :: Definition -> IO Definition
readDef def =
  let
    doRead  = Text.readFile $ absolutePath def
    doStore = \c -> def { content = Just (Text.unpack c) }
  in
    doRead <&> doStore


{-| Rename
-}
rename :: FilePath -> FilePath -> IO Dictionary -> IO Dictionary
rename oldPath newPath = fmap $ List.map (renameDef oldPath newPath)


renameDef :: FilePath -> FilePath -> Definition -> Definition
renameDef oldPath newPath def =
  if (localPath def) == oldPath
    then forkDefinition newPath def
    else def


{-| Write
-}
write :: FilePath -> IO Dictionary -> IO Dictionary
write destination = (=<<) $ List.map (writeDef destination) .> sequence


writeDef :: FilePath -> Definition -> IO Definition
writeDef destination def =
  let
    destination'    = combine (rootPath def) destination
    finalFilePath   = combine destination' (localPath def)
    content'        = fromMaybe "" (content def)

    createDirIO     = createDirectoryIfMissing True (takeDirectory finalFilePath)
    writeFileIO     = Text.writeFile finalFilePath (Text.pack content')
  in
    createDirIO >> writeFileIO >> (return def)
