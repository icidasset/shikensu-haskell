module Shikensu.Contrib.Pure where


{-| Resources

Inspiration
https://github.com/elm-lang/elm-make
https://github.com/elm-lang/elm-compiler

File IO, writing Strings to disk
https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text-Lazy-IO.html

-}

import Debug.Trace (traceIO)
import Flow
import Shikensu (absolutePath, localPath)
import Shikensu.Types
import System.Directory (createDirectory)
import System.FilePath (combine, takeDirectory)

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text (readFile, writeFile)


{-| Read
-}
read :: Dictionary -> Dictionary
read =
  fmap $ \def -> def {
    content = fmap Text.unpack (Text.readFile $ absolutePath def)
  }


{-| Write
-}
write :: String -> Dictionary -> Dictionary
write destination dictionary =
  (\def -> def {
    content =
      let
        destination'    = combine (rootPath def) destination
        createDirIO     = createDirectory destination'
        finalFilePath   = combine destination' (localPath def)
        writeFileFn     = \content -> Text.writeFile finalFilePath (Text.pack content)
        writeFileIO     = (content def) >>= writeFileFn
      in
        (traceIO "WHY-U-NO-SHOW?!?")
          >> createDirIO
          >> writeFileIO
          >> content def
  })

  `fmap`

  dictionary
