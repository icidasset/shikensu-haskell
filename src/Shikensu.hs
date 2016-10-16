module Shikensu where


{-| Shikensu.

## How to use

    import Shikensu (list)
    import Shikensu.Types (Definition, Dictionary)
    import Shikensu.Contrib (read, rename, write)

    dictionary_io :: IO Dictionary
    dictionary_io =
      Shikensu.list ["**/*.html"] absolute_path


    dictionary_io
      |> read
      |> rename "main.html" "index.html"
      |> write "./build"


    dictionary_io
      |> Shikensu.io a
      |> Shikensu.pure b

      -- which is basically

      |> (=<<) (sequence . a)
      |> fmap b


    -- Where a :: [Definition] -> [IO Definition]
    --       b :: [Definition] -> [Definition]
    --
    -- type alias Dictionary = [Definition]


    dictionary_io
      |> Shikensu.mapIO x
      |> Shikensu.mapPure y

    -- Where x :: Definition -> IO Definition
    --       y :: Definition -> Definition

-}

import Flow
import Shikensu.Types
import Shikensu.Utilities
import System.FilePath

import qualified Data.List as List (concat, map, zip)
import qualified Data.Map.Lazy as Map (empty)



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



{-| Fork a Definition.
-}
forkDefinition :: FilePath -> Definition -> Definition
forkDefinition newLocalPath def =
  let
    dirname = replaceSingleDot (takeDirectory newLocalPath)
  in
    Definition
      { basename = takeBaseName newLocalPath
      , dirname = dirname
      , extname = takeExtension newLocalPath
      , pattern = (pattern def)
      , rootPath = (rootPath def)
      , workingDirectory = (workingDirectory def)

      -- Additional properties
      , content = (content def)
      , metadata = (metadata def)
      , parentPath = compileParentPath dirname
      , pathToRoot = compilePathToRoot dirname
      }



{-| Make a Definition.

Example definition, given:
- the absolute path `/Users/icidasset/Projects/shikensu/example/test/hello.md`
- {deps} the root path `/Users/icidasset/Projects/shikensu`
- {deps} the pattern `example/**/*.md`

    Definition
      { basename = "hello"
      , dirname = "test"
      , extname = ".md"
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

    absPath'        = normalise absPath
    workingDir      = cleanPath (commonDirectory pattern)
    absWorkingDir   = joinPath [rootPath, workingDir]
    localPath       = cleanPath (stripPrefix absWorkingDir absPath')
    dirname         = replaceSingleDot (takeDirectory localPath)
  in
    Definition
      { basename = takeBaseName localPath
      , dirname = dirname
      , extname = takeExtension localPath
      , pattern = pattern
      , rootPath = rootPath
      , workingDirectory = workingDir

      -- Additional properties
      , content = Nothing
      , metadata = Map.empty
      , parentPath = compileParentPath dirname
      , pathToRoot = compilePathToRoot dirname
      }



{-| Make a Dictionary.
-}
makeDictionary :: FilePath -> (Pattern, [FilePath]) -> Dictionary
makeDictionary rootPath (pattern, files) =
  makeDefinition Dependencies { _pattern = pattern, _rootPath = rootPath } <$> files




-- Sequence functions
--   (See the documentation above and the Contrib module for more info)


io :: ([Definition] -> [IO Definition]) -> IO Dictionary -> IO Dictionary
io fn = (=<<) (sequence . fn)


pure :: ([Definition] -> [Definition]) -> IO Dictionary -> IO Dictionary
pure fn = (<$>) (fn)


mapIO :: (Definition -> IO Definition) -> IO Dictionary -> IO Dictionary
mapIO = (Shikensu.io . List.map)


mapPure :: (Definition -> Definition) -> IO Dictionary -> IO Dictionary
mapPure = (Shikensu.pure . List.map)




-- Path functions


absolutePath :: Definition -> String
absolutePath def =
  joinPath [rootPath def, workingDirectory def, localPath def]


localPath :: Definition -> String
localPath def =
  joinPath [dirname def, (basename def) ++ (extname def)]




-- Sorting functions
--   (e.g. `Data.List.sortBy Shikensu.sortByAbsolutePath dictionary`)


sortByAbsolutePath :: Definition -> Definition -> Ordering
sortByAbsolutePath defA defB =
  compare (absolutePath defA) (absolutePath defB)
