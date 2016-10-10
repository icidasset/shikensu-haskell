module Shikensu.Contrib.Pure where


{-| Resources

Inspiration
https://github.com/elm-lang/elm-make
https://github.com/elm-lang/elm-compiler

File IO, writing Strings to disk
https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text-Lazy-IO.html

-}

import Flow
import Shikensu.Types

import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text (readFile)


{-| Read
-}
read :: Dictionary -> Dictionary
read =
  fmap $ \def -> def {
    content = fmap Text.unpack (Text.readFile $ absolutePath def)
  }
