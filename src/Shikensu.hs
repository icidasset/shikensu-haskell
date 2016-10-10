module Shikensu where


{-| Shikensu.

## How to use

    import Shikensu (list)
    import Shikensu.Types (Dictionary)
    import Shikensu.Contrib.IO (read, rename, write)

    dictionary_io :: IO Dictionary
    dictionary_io =
      Shikensu.list ["**/*.html"] absolute_path

    dictionary_io
      |> read
      |> rename "main.html" "index.html"
      |> write "./build"

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

      , content = IO ""
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
      { absolutePath = absPath'
      , basename = takeBaseName localPath
      , dirname = dirname
      , extname = takeExtension localPath
      , localPath = localPath
      , pattern = pattern
      , rootPath = rootPath
      , workingDirectory = workingDir

      -- Additional properties
      , content = emptyContent
      , metadata = Map.empty
      , parentPath = compileParentPath dirname
      , pathToRoot = compilePathToRoot dirname
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




-- Sorting functions
--   (e.g. `Data.List.sortBy Shikensu.sortByAbsolutePath dictionary`)


sortByAbsolutePath :: Definition -> Definition -> Ordering
sortByAbsolutePath defA defB =
  compare (absolutePath defA) (absolutePath defB)
