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
    , module Shikensu.Internal.Types
    ) where

import Data.Monoid ((<>))
import Flow
import Shikensu.Internal.Types
import Shikensu.Internal.Utilities
import System.FilePath

import qualified Data.Aeson.KeyMap as KeyMap (empty)
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
- the absolute path `/Users/icidasset/Projects/shikensu/example/test/hello.md`

> Definition
>     { basename = "hello"
>     , dirname = "test"
>     , extname = ".md"
>     , pattern = "example/**/*.md"
>     , rootDirname = "/Users/icidasset/Projects/shikensu"
>     , workingDirname = "example"
>
>     , content = Nothing
>     , metadata = KeyMap.empty
>     , parentPath = "../"
>     , pathToRoot = "../../"
>     }

-}
makeDefinition :: FilePath -> String -> FilePath -> Definition
makeDefinition rootDirname pattern absolutePath =
    let
        workingDirname      = commonDirectory pattern
        rootWorkingDirname  = (combine rootDirname workingDirname) <> [ pathSeparator ]

        theAbsolutePath     = normalise absolutePath
        theLocalPath        = stripPrefix rootWorkingDirname theAbsolutePath
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
            , metadata        = KeyMap.empty
            , parentPath      = compileParentPath $ takeDirName theLocalPath
            , pathToRoot      = compilePathToRoot $ takeDirName theLocalPath
            }



{-| Make a Dictionary.
-}
makeDictionary :: FilePath -> (String, [FilePath]) -> Dictionary
makeDictionary rootDirname (pattern, filepaths) =
    List.map (makeDefinition rootDirname pattern) filepaths
