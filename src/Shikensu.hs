module Shikensu where


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

import Data.Map.Lazy (Map)
import Data.Maybe (fromJust)
import Data.String
import Data.Tuple (fst, snd)
import System.FilePath

import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import qualified System.FilePath.Glob as Glob


data Definition =
  Definition
    { absolutePath :: FilePath
    , basename :: String
    , dirname :: FilePath
    , extname :: String
    , localPath :: FilePath
    , pattern :: Pattern
    , root :: FilePath
    , wd :: FilePath

    -- Additional properties
    , content :: Maybe String
    , metadata :: Map String String
    , parentPath :: Maybe FilePath
    , pathToRoot :: FilePath
    } deriving (Show)


type Dictionary = [Definition]
type Pattern = String


{-| Make a single dictionary based on multiple glob patterns and a path to a directory.
-}
list :: [Pattern] -> FilePath -> IO Dictionary
list patterns rootPath =
  patterns
    |> compilePatterns
    |> globDir rootPath
    |> fmap (List.zip patterns)
    |> fmap (List.map (makeDictionary rootPath))
    |> fmap (List.concat)



-- Definitions & Dictionary functions


makeDefinition :: Pattern -> FilePath -> FilePath -> Definition
makeDefinition pattern rootPath absPath =
  let
    workingDir  = (cleanPath . fst) (Glob.commonDirectory (Glob.compile pattern))
    localPath   = (cleanPath . fromJust) (List.stripPrefix rootPath absPath)
    dirname     = (cleanPath . fromJust) (List.stripPrefix workingDir (takeDirectory localPath))
  in
    Definition
      { absolutePath = absPath
      , basename = takeBaseName localPath
      , dirname = dirname
      , extname = takeExtension localPath
      , localPath = localPath
      , pattern = pattern
      , root = rootPath
      , wd = workingDir

      -- Additional properties
      , content = Nothing
      , metadata = Map.empty
      , parentPath = Nothing
      , pathToRoot = "./"
      }


makeDictionary :: FilePath -> (Pattern, [FilePath]) -> Dictionary
makeDictionary rootPath fileListWithPattern =
  let
    pattern = fst fileListWithPattern
    fileList = snd fileListWithPattern
  in
    map (makeDefinition pattern rootPath) fileList


-- Utility functions


cleanPath :: FilePath -> FilePath
cleanPath = (dropDrive . dropTrailingPathSeparator . normalise)


compilePatterns :: [Pattern] -> [Glob.Pattern]
compilePatterns = List.map Glob.compile


globDir :: FilePath -> [Glob.Pattern] -> IO [[FilePath]]
globDir a b = fmap fst (Glob.globDir b a)
