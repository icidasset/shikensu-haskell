module Shikensu.Contrib.IO
  ( Shikensu.Contrib.IO.read
  , Shikensu.Contrib.IO.readDef
  , Shikensu.Contrib.IO.write
  , Shikensu.Contrib.IO.writeDef
  ) where

import Data.Maybe (fromMaybe)
import Flow
import Shikensu.Types
import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, joinPath, takeDirectory)

import qualified Data.ByteString as B (empty, readFile, writeFile)
import qualified Shikensu.Utilities as Utilities (mapIO)


{-| Read.

Read the contents of each file, and for each file (ie. definition)
put that content in the `content` property.
-}
read :: Dictionary -> IO Dictionary
read = Utilities.mapIO (readDef)


readDef :: Definition -> IO Definition
readDef def =
  fmap
    (\c -> def { content = Just c })
    (B.readFile $ absolutePath def)



{-| Write.

Write the contents of each definition to a file.
The path of the new file is `joinPath [rootDirname, givenDirectoryName, localPath]`.
-}
write :: FilePath -> Dictionary -> IO Dictionary
write dest = Utilities.mapIO (writeDef dest)


writeDef :: FilePath -> Definition -> IO Definition
writeDef dest def =
  let
    path = joinPath [rootDirname def, dest, localPath def]
    cont = fromMaybe B.empty (content def)
  in
    createDirectoryIfMissing True (takeDirectory path)
    >> B.writeFile path cont
    >> return def
