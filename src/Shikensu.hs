module Shikensu where


{-| Shikensu.

## How to use

    import Shikensu (list)
    import Shikensu.Contrib.IO (read, rename, write)

    dictionary_io =
      Shikensu.list ["**/*.html"] absolute_path

    dictionary_io
      |> read
      |> rename "main.html" "index.html"
      |> write "./build"


## Additional info

The code in this module is written in order of importance.
That is:

1. Records and types
2. Functions you will use all the time
3. Functions you will sometimes use
4. Functions you will seldom use
5. ...

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
    , content :: IO String
    , metadata :: Map String String
    , parentPath :: Maybe FilePath
    , pathToRoot :: FilePath
    }


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


{-| Make a single dictionary based on multiple glob patterns and a path to a directory.

1. Compile patterns so `globDir` can use them.
2. Run `globDir` function on the given (root) path.
3. We get a list back for each pattern (ie. a list of lists),
   here we put each child list in a tuple along with its pattern.
4. We make a Dictionary out of each tuple (this also needs the path).
5. Merge the dictionaries into one dictionary.

-}
list :: [Pattern] -> FilePath -> IO Dictionary
list patterns rootPath =
  patterns
    |> compilePatterns
    |> globDir rootPath
    |> fmap (List.zip patterns)
    |> fmap (List.map (makeDictionary rootPath))
    |> fmap (List.concat)



-- Sorting functions
--   (e.g. `List.sortBy Shikensu.sortByAbsolutePath dictionary`)


sortByAbsolutePath :: Definition -> Definition -> Ordering
sortByAbsolutePath defA defB =
  compare (absolutePath defA) (absolutePath defB)



-- Definitions & Dictionary functions


{-| Make a Definition.

Example definition, given:
- the absolute path `/Users/icidasset/Projects/shikensu/example/test/hello.md`
- {deps} the root path `/Users/icidasset/Projects/shikensu`
- {deps} the pattern `example/**/*.md`

    Definition
      { absolutePath = "/Users/icidasset/Projects/shikensu/example/test/hello.md"
      , basename = "hello"
      , dirname = "test"
      , extname = "md"
      , localPath = "test/hello.md"
      , pattern = "example/**/*.md"
      , rootPath = "/Users/icidasset/Projects/shikensu"
      , workingDirectory = "example"

      , content = Nothing
      , metadata = Map.empty
      , parentPath = "../"
      , pathToRoot = "../../"
      }

-}
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

    dirname         = takeDirectory localPath
    dirname'        = if dirname == "." then "" else dirname
  in
    Definition
      { absolutePath = absPath
      , basename = takeBaseName localPath
      , dirname = dirname'
      , extname = takeExtension localPath
      , localPath = localPath
      , pattern = pattern
      , rootPath = rootPath
      , workingDirectory = workingDir

      -- Additional properties
      , content = emptyContent
      , metadata = Map.empty
      , parentPath = compileParentPath dirname'
      , pathToRoot = compilePathToRoot dirname'
      }


{-| Make a Dictionary
-}
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


{-| Path to parent, when there is one.

    Just "../" or Nothing

-}
compileParentPath :: FilePath -> Maybe FilePath
compileParentPath dirname =
  case dirname of
    "" -> Nothing
    _  -> Just "../"


{-| Path to root.

Example, if `dirname` is 'example/subdir',
then this will be `../../`.

If the `dirname` is empty,
then this will be empty as well.

-}
compilePathToRoot :: FilePath -> FilePath
compilePathToRoot dirname =
  if dirname == "" then
    ""
  else
    dirname
      |> splitDirectories
      |> fmap (\_ -> "..")
      |> joinPath
      |> addTrailingPathSeparator


{-| No content.
-}
emptyContent :: IO String
emptyContent = return ""


{-| List contents of a directory using a glob pattern.
-}
globDir :: FilePath -> [Glob.Pattern] -> IO [[FilePath]]
globDir a b = fmap fst (Glob.globDir b a)
