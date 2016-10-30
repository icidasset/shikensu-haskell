{-# LANGUAGE OverloadedStrings #-}
module Shikensu
  ( list
  , forkDefinition
  , makeDefinition
  , makeDictionary
  , Shikensu.io
  , Shikensu.pure
  , mapIO
  , mapPure
  , absolutePath
  , localPath
  , transposeMetadata
  , transposeToMetadata
  , sortByAbsolutePath
  ) where


{-| Shikensu.

## How to use

    import Shikensu (list)
    import Shikensu.Types (Definition, Dictionary)
    import Shikensu.Contrib (read, rename, write)

    dictionary_io :: IO Dictionary
    dictionary_io =
      Shikensu.list ["**/*.html"] absolute_path_to_cwd


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

import qualified Data.Aeson as Aeson (FromJSON, Result(..), ToJSON, fromJSON, toJSON)
import qualified Data.HashMap.Strict as HashMap (empty)
import qualified Data.List as List (concat, map, zip)



{-| Make a single dictionary based on multiple glob patterns and a path to a directory.

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
    |> fmap (List.map (makeDictionary rootDir))
    |> fmap (List.concat)



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

    Definition
      { basename = "hello"
      , dirname = "test"
      , extname = ".md"
      , pattern = "example/**/*.md"
      , rootDirname = "/Users/icidasset/Projects/shikensu"
      , workingDirname = "example"

      , content = Nothing
      , metadata = HashMap.empty
      , parentPath = "../"
      , pathToRoot = "../../"
      }

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
makeDictionary :: FilePath -> (Pattern, [FilePath]) -> Dictionary
makeDictionary _rootDirname (_pattern, files) =
  makeDefinition _rootDirname _pattern <$> files




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
  joinPath [rootDirname def, workspacePath def]


localPath :: Definition -> String
localPath def =
  joinPath [dirname def, (basename def) ++ (extname def)]


workspacePath :: Definition -> String
workspacePath def =
  joinPath [workingDirname def, localPath def]




-- Metadata functions


{-| Transpose metadata.

Transpose our metadata object to a given type
which implements the Aeson.FromJSON instance.

    data Example =
      Example { some :: Text }
      deriving (Generic, FromJSON)

    hashMap     = HashMap.fromList [ ("some", "metadata") ]
    defaultEx   = Example { some = "default" }
    example     = transposeMetadata hashMap defaultExample :: Example

-}
transposeMetadata :: Aeson.FromJSON a => Metadata -> a -> a
transposeMetadata hashMap fallback =
  let
    result = hashMap
      |> Aeson.toJSON
      |> Aeson.fromJSON :: Aeson.FromJSON b => Aeson.Result b
  in
    case result of
      Aeson.Success x -> x
      Aeson.Error _   -> fallback


{-| Inverse of `transposeMetadata`.
-}
transposeToMetadata :: (Aeson.ToJSON a, Aeson.FromJSON a) => a -> Metadata
transposeToMetadata generic =
  let
    result = generic
      |> Aeson.toJSON
      |> Aeson.fromJSON :: Aeson.FromJSON b => Aeson.Result b
  in
    case result of
      Aeson.Success x -> x
      Aeson.Error _   -> HashMap.empty




-- Sorting functions
--   (e.g. `Data.List.sortBy Shikensu.sortByAbsolutePath dictionary`)


sortByAbsolutePath :: Definition -> Definition -> Ordering
sortByAbsolutePath defA defB =
  compare (absolutePath defA) (absolutePath defB)
