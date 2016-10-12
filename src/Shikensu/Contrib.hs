module Shikensu.Contrib (
    Shikensu.Contrib.read
  , Shikensu.Contrib.write
  ) where

import Data.Maybe (fromMaybe)
import Flow
import Shikensu (absolutePath, localPath)
import Shikensu.Types
import System.Directory (createDirectoryIfMissing)
import System.FilePath (FilePath, combine, takeDirectory)

import qualified Data.List as List
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text (readFile, writeFile)


{-| Read
-}
read :: IO Dictionary -> IO Dictionary
read a = a >>= read_


read_ :: Dictionary -> IO Dictionary
read_ dict =
  dict
    |> List.map (\def ->
        fmap
          (\c -> def { content = Just (Text.unpack c) })
          (Text.readFile $ absolutePath def)
       )
    |> sequence



{-| Write
-}
write :: FilePath -> IO Dictionary -> IO Dictionary
write a b = b >>= write_ a


write_ :: FilePath -> Dictionary -> IO Dictionary
write_ destination dict =
  dict
    |> List.map (\def ->
        let
          destination'    = combine (rootPath def) destination
          finalFilePath   = combine destination' (localPath def)
          content'        = fromMaybe "" (content def)

          createDirIO     = createDirectoryIfMissing True (takeDirectory finalFilePath)
          writeFileIO     = Text.writeFile finalFilePath (Text.pack content')
        in
          fmap (\() -> def) (createDirIO >> writeFileIO)
       )
    |> sequence
