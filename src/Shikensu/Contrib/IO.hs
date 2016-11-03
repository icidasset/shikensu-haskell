{-# LANGUAGE OverloadedStrings #-}
module Shikensu.Contrib.IO
  ( Shikensu.Contrib.IO.read
  , Shikensu.Contrib.IO.readDef
  , Shikensu.Contrib.IO.write
  , Shikensu.Contrib.IO.writeDef
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Flow
import Shikensu.Types
import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, joinPath, takeDirectory)

import qualified Data.Text.IO as Text (readFile, writeFile)
import qualified Shikensu.Utilities as Utilities (mapIO)


{-| Read
-}
read :: Dictionary -> IO Dictionary
read = Utilities.mapIO (readDef)


readDef :: Definition -> IO Definition
readDef def =
  fmap
    (\c -> def { content = Just c })
    (Text.readFile $ absolutePath def)



{-| Write
-}
write :: FilePath -> Dictionary -> IO Dictionary
write dest = Utilities.mapIO (writeDef dest)


writeDef :: FilePath -> Definition -> IO Definition
writeDef dest def =
  let
    path = joinPath [rootDirname def, dest, localPath def]
    cont = fromMaybe "" (content def)
  in
    createDirectoryIfMissing True (takeDirectory path)
    >> Text.writeFile path cont
    >> return def
