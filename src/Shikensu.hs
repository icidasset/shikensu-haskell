{-| Shikensu.

See the README and tests for examples.

-}
module Shikensu
    ( list
    , listF
    , listRelative
    , listRelativeF

    , forkDefinition
    , makeDefinition
    , makeDictionary
    ) where

import Flow
import Shikensu.Internal.Utilities
import Shikensu.Types
import System.FilePath

import qualified Data.HashMap.Strict as HashMap (empty)
import qualified Data.List as List (concat, map, zip)
import qualified System.Directory as Dir (canonicalizePath)



-- IO


{-| Make a single dictionary based on a path to a directory and multiple glob patterns.

1. Compile patterns so `globDir` can use them.
2. Run `globDir` function on the given (root) path.
3. We get a list back for each pattern (ie. a list of lists),
   here we put each child list in a tuple along with its pattern.
4. We make a Dictionary out of each tuple (this also needs the path).
5. Merge the dictionaries into one dictionary.

-}
list :: [Pattern] -> FilePath -> IO Dictionary
list patterns rootDir =
    patterns
        |> compilePatterns
        |> globDir rootDir
        |> fmap (List.zip patterns)
        |> fmap (List.map (uncurry . makeDictionary $ rootDir))
        |> fmap (List.concat)


{-| Flipped version of `list`.
-}
listF :: FilePath -> [Pattern] -> IO Dictionary
listF = flip list


{-| Same as `list`, but given a relative directory.

> listRelative ["*.md"] "./articles"
-}
listRelative :: [Pattern] -> FilePath -> IO Dictionary
listRelative patterns relativePath =
    Dir.canonicalizePath relativePath >>= list patterns


{-| Flipped version `listRelative`.
-}
listRelativeF :: FilePath -> [Pattern] -> IO Dictionary
listRelativeF = flip listRelative




-- PURE


{-| Fork a Definition.
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

Example definition, given:

- the root path `/Users/icidasset/Projects/shikensu`
- the pattern `example/**/*.md`
- the workspace path `example/test/hello.md`

> Definition
>     { basename = "hello"
>     , dirname = "test"
>     , extname = ".md"
>     , pattern = "example/**/*.md"
>     , rootDirname = "/Users/icidasset/Projects/shikensu"
>     , workingDirname = "example"
>
>     , content = Nothing
>     , metadata = HashMap.empty
>     , parentPath = "../"
>     , pathToRoot = "../../"
>     }

-}
makeDefinition :: FilePath -> Pattern -> FilePath -> Definition
makeDefinition _rootDirname _pattern _workspacePath =
    let
        workingDir  = cleanPath . (commonDirectory) $ _pattern
        localPath   = cleanPath . (stripPrefix workingDir) . cleanPath $ _workspacePath
    in
        Definition
            { basename        = takeBaseName localPath
            , dirname         = takeDirName localPath
            , extname         = takeExtension localPath
            , pattern         = _pattern
            , rootDirname     = dropTrailingPathSeparator _rootDirname
            , workingDirname  = workingDir

            -- Additional properties
            , content         = Nothing
            , metadata        = HashMap.empty
            , parentPath      = compileParentPath $ takeDirName localPath
            , pathToRoot      = compilePathToRoot $ takeDirName localPath
            }



{-| Make a Dictionary.
-}
makeDictionary :: FilePath -> Pattern -> [FilePath] -> Dictionary
makeDictionary _rootDirname _pattern =
    List.map (makeDefinition _rootDirname _pattern)
