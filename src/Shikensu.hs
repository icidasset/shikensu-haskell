{-| Shikensu.

See the README and tests for examples.

-}
module Shikensu
    ( forkDefinition
    , list
    , listF
    , listRelative
    , listRelativeF
    , makeDefinition
    , makeDictionary
    ) where

import Flow
import Shikensu.Internal.Utilities
import Shikensu.Types
import System.FilePath

import qualified Data.HashMap.Strict as HashMap (empty)
import qualified Data.List as List (concatMap, map, zip)
import qualified System.Directory as Dir (canonicalizePath)
import qualified System.FilePath.Glob as Glob (compile, globDir1)


-- IO


{-| Make a single dictionary based on a path to a directory and multiple glob patterns.

1. Compile patterns so `globDir` can use them.
2. Run `globDir` function on the given (root) path.
3. We get a list back for each pattern (ie. a list of lists),
   here we put each child list in a tuple along with its pattern.
4. We make a Dictionary out of each tuple (this also needs the path).
5. Merge the dictionaries into one dictionary.

> list ["*.md"] "/root/articles"


## Tests

$setup
>>> import qualified Test.Utils

Standard usage.

>>> Test.Utils.printDF $ listRelative ["tests/**/*.md"] "."
Definition
  { basename = "example"
  , dirname = "fixtures"
  , extname = ".md"
  , pattern = "tests/**/*.md"
  , rootDirname = ...
  , workingDirname = "tests"
  , content = ...
  , metadata = ...
  , parentPath = Just "../"
  , pathToRoot = "../"
  }

Pattern with a leading dot-slash.

>>> Test.Utils.printDF $ listRelative ["./tests/**/*.md"] "."
Definition
  { basename = "example"
  , dirname = "fixtures"
  , extname = ".md"
  , pattern = "./tests/**/*.md"
  , rootDirname = ...
  , workingDirname = "tests"
  , content = ...
  , metadata = ...
  , parentPath = Just "../"
  , pathToRoot = "../"
  }

Pattern without a "common directory" (workingdirname).

>>> Test.Utils.printDF $ listRelative ["**/*.md"] "./tests"
Definition
  { basename = "example"
  , dirname = "fixtures"
  , extname = ".md"
  , pattern = "**/*.md"
  , rootDirname = ...
  , workingDirname = ""
  , content = ...
  , metadata = ...
  , parentPath = Just "../"
  , pathToRoot = "../"
  }

Test file in the root directory.

>>> Test.Utils.printDF $ listRelative ["*.md"] "./"
Definition
  { basename = "CHANGELOG"
  , dirname = ""
  , extname = ".md"
  , pattern = "*.md"
  , rootDirname = ...
  , workingDirname = ""
  , content = ...
  , metadata = ...
  , parentPath = Nothing
  , pathToRoot = ""
  }

Test `list` with a root dir that has a trailing slash.

>>> import Data.Monoid ((<>))
>>> import System.Directory (canonicalizePath)
>>> :{
        fmap (<> "/") (canonicalizePath ".")
            >>= list ["./*.md"]
            |> Test.Utils.printDF
    :}
Definition
  { basename = "CHANGELOG"
  , dirname = ""
  , extname = ".md"
  , pattern = "./*.md"
  , rootDirname = ...
  , workingDirname = ""
  , content = ...
  , metadata = ...
  , parentPath = Nothing
  , pathToRoot = ""
  }

-}
list :: [String] -> FilePath -> IO Dictionary
list patterns rootDir =
    patterns
        |> List.map (Glob.compile)
        |> List.map (flip Glob.globDir1 $ rootDir)
        |> sequence
        |> fmap (List.zip patterns)
        |> fmap (List.concatMap $ makeDictionary rootDir)


{-| Flipped version of `list`.
-}
listF :: FilePath -> [String] -> IO Dictionary
listF = flip list


{-| Same as `list`, but given a relative directory.

> listRelative ["*.md"] "./articles"

-}
listRelative :: [String] -> FilePath -> IO Dictionary
listRelative patterns relativePath =
    Dir.canonicalizePath relativePath >>= list patterns


{-| Flipped version `listRelative`.
-}
listRelativeF :: FilePath -> [String] -> IO Dictionary
listRelativeF = flip listRelative



-- PURE


{-| Fork a Definition.

>>> import qualified Test.Utils
>>> :{
        "/root/src/Example.hs"
            |> makeDefinition "/root" "*.*"
            |> forkDefinition "new_path/Sample.hs"
            |> Test.Utils.print
    :}
Definition
  { basename = "Sample"
  , dirname = "new_path"
  , extname = ".hs"
  , pattern = "*.*"
  , rootDirname = "/root"
  , workingDirname = ""
  , content = Nothing
  , metadata = fromList []
  , parentPath = Just "../"
  , pathToRoot = "../"
  }

-}
forkDefinition :: FilePath -> Definition -> Definition
forkDefinition newLocalPath def =
    Definition
        { basename        = takeBaseName newLocalPath
        , dirname         = takeDirName newLocalPath
        , extname         = takeExtension newLocalPath
        , pattern         = (pattern def)
        , rootDirname     = (rootDirname def)
        , workingDirname  = (workingDirname def)

        -- Additional properties
        , content         = (content def)
        , metadata        = (metadata def)
        , parentPath      = compileParentPath $ takeDirName newLocalPath
        , pathToRoot      = compilePathToRoot $ takeDirName newLocalPath
        }



{-| Make a Definition.

>>> import qualified Test.Utils
>>> :{
        "/root/src/Shikensu/Contrib.hs"
            |> makeDefinition "/root" "src/**/*.hs"
            |> Test.Utils.print
    :}
Definition
  { basename = "Contrib"
  , dirname = "Shikensu"
  , extname = ".hs"
  , pattern = "src/**/*.hs"
  , rootDirname = "/root"
  , workingDirname = "src"
  , content = Nothing
  , metadata = fromList []
  , parentPath = Just "../"
  , pathToRoot = "../"
  }

-}
makeDefinition :: FilePath -> String -> FilePath -> Definition
makeDefinition rootDirname pattern absolutePath =
    let
        workingDirname      = commonDirectory pattern
        rootWorkingDirname  = combine rootDirname workingDirname

        theAbsolutePath     = normalise absolutePath
        theLocalPath        = dropDrive (stripPrefix rootWorkingDirname theAbsolutePath)
    in
        Definition
            { basename        = takeBaseName theLocalPath
            , dirname         = takeDirName theLocalPath
            , extname         = takeExtension theLocalPath
            , pattern         = pattern
            , rootDirname     = rootDirname
            , workingDirname  = workingDirname

            -- Additional properties
            , content         = Nothing
            , metadata        = HashMap.empty
            , parentPath      = compileParentPath $ takeDirName theLocalPath
            , pathToRoot      = compilePathToRoot $ takeDirName theLocalPath
            }



{-| Make a Dictionary.
-}
makeDictionary :: FilePath -> (String, [FilePath]) -> Dictionary
makeDictionary rootDirname (pattern, filepaths) =
    List.map (makeDefinition rootDirname pattern) filepaths
