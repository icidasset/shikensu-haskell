module Shikensu (Definition, Dictionary, list) where


{-| How to use

    dictionary_io =
      Shikensu.list ["**/*.html"] absolute_path

    dictionary_io
      |> read
      |> rename "main.html" "index.html"
      |> write "./build"

-}


{-| Resources

Inspiration
https://github.com/elm-lang/elm-make
https://github.com/elm-lang/elm-compiler

File IO, writing Strings to disk
https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text-Lazy-IO.html

-}

import Flow

import Debug.Trace
import Data.Map.Lazy (Map)
import Data.Maybe (fromMaybe)
import Data.String
import Data.Tuple
import System.FilePath

import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import qualified System.FilePath.Glob as Glob


data Definition =
  Definition
    { basename :: String
    , dirname :: FilePath
    , entirePath :: FilePath
    , extname :: String
    , path :: FilePath
    , pattern :: Maybe String
    , root :: FilePath
    , wd :: FilePath

    -- Additional properties
    , content :: Maybe String
    , metadata :: Map String String
    , parentPath :: Maybe FilePath
    , pathToRoot :: FilePath
    }


type Dictionary = [Definition]


{-| Make a dictionary based on a given path and pattern.
-}
list :: [String] -> FilePath -> IO Dictionary
list patterns rootPath =
  let
    patterns' = map Glob.compile patterns
    matchingFilePaths = fmap fst (Glob.globDir patterns' rootPath)
  in
    fmap (List.concat .> makeDictionary) matchingFilePaths



-- Private


makeDictionary :: [FilePath] -> Dictionary
makeDictionary fileList =
  map makeDefinition fileList


makeDefinition :: FilePath -> Definition
makeDefinition filePath =
  Definition
    { basename = takeBaseName filePath
    , dirname = takeDirectory filePath
    , entirePath = filePath
    , extname = takeExtension filePath
    , path = filePath
    , pattern = Just "TODO"
    , root = "TODO"
    , wd = "TODO"

    -- Additional properties
    , content = Nothing
    , metadata = Map.empty
    , parentPath = Nothing
    , pathToRoot = "./"
    }
