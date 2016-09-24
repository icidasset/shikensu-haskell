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


{-| A file definition, along with some additional properties.
-}
data Definition =
  Definition
    { absolutePath :: FilePath
    , basename :: String
    , dirname :: FilePath
    , extname :: String
    , localPath :: FilePath
    , pattern :: Pattern
    , rootPath :: FilePath
    , workingDirectory :: FilePath

    -- Additional properties
    , content :: Maybe String
    , metadata :: Map String String
    , parentPath :: Maybe FilePath
    , pathToRoot :: FilePath
    } deriving (Show)


{-| Definition dependencies.

A set of properties all definitions from a `Dictionary` produced
by `makeDictionary` have in common.
-}
data Dependencies =
  Dependencies
    { _pattern :: String
    , _rootPath :: FilePath
    }



-- Type aliases


type Dictionary = [Definition]
type Pattern = String



-- Main functions
-- (ie. functions that you will use the most)


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


makeDefinition :: Dependencies -> FilePath -> Definition
makeDefinition deps absPath =
  let
    pattern         = _pattern deps
    rootPath        = _rootPath deps

    -- The following assumes that absPath and rootPath have a format of `/a/b/c(/)`
    -- (ie. for POSIX systems)

    workingDir      = (cleanPath <. fst) (Glob.commonDirectory (Glob.compile pattern))
    absWorkingDir   = (joinPath) [rootPath, workingDir]
    localPath       = (cleanPath <. fromJust) (List.stripPrefix absWorkingDir absPath)
  in
    Definition
      { absolutePath = absPath
      , basename = takeBaseName localPath
      , dirname = takeDirectory localPath
      , extname = takeExtension localPath
      , localPath = localPath
      , pattern = pattern
      , rootPath = rootPath
      , workingDirectory = workingDir

      -- Additional properties
      , content = Nothing
      , metadata = Map.empty
      , parentPath = Nothing
      , pathToRoot = "./TODO"
      }


makeDictionary :: FilePath -> (Pattern, [FilePath]) -> Dictionary
makeDictionary rootPath patternAndFileList =
  let
    pattern = fst patternAndFileList
    fileList = snd patternAndFileList
    deps = Dependencies { _pattern = pattern, _rootPath = rootPath }
  in
    map (makeDefinition deps) fileList



-- Utility functions


cleanPath :: FilePath -> FilePath
cleanPath = (normalise .> dropTrailingPathSeparator .> dropDrive)


compilePatterns :: [Pattern] -> [Glob.Pattern]
compilePatterns = List.map Glob.compile


globDir :: FilePath -> [Glob.Pattern] -> IO [[FilePath]]
globDir a b = fmap fst (Glob.globDir b a)
