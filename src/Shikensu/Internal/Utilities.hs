{-| Internal utility functions.

-}
module Shikensu.Internal.Utilities
    ( commonDirectory
    , compileParentPath
    , compilePathToRoot
    , replaceSingleDot
    , stripPrefix
    , takeDirName
    ) where

import Data.Maybe (fromMaybe)
import Flow
import System.FilePath

import qualified Data.List as List (map, stripPrefix)
import qualified Data.Tuple as Tuple (fst)
import qualified System.FilePath.Glob as Glob


{-| Get the common directory from a pattern.
-}
commonDirectory :: String -> FilePath
commonDirectory =
    Glob.compile
    .> Glob.commonDirectory
    .> Tuple.fst
    .> normalise
    .> dropTrailingPathSeparator
    .> replaceSingleDot


{-| Path to parent, when there is one.

> Just "../" or Nothing

-}
compileParentPath :: FilePath -> Maybe FilePath
compileParentPath dirname =
    case dirname of
        "" -> Nothing
        _  -> Just $ addTrailingPathSeparator ".."


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
            |> fmap (const "..")
            |> joinPath
            |> addTrailingPathSeparator


{-| If the path is a single dot, return an empty string.
Otherwise return the path.
-}
replaceSingleDot :: String -> String
replaceSingleDot path =
    if path == "." then "" else path


{-| Strip prefix.
-}
stripPrefix :: String -> String -> String
stripPrefix prefix target =
    fromMaybe target (List.stripPrefix prefix target)


{-| Take dirname and replace single dot.
-}
takeDirName :: FilePath -> FilePath
takeDirName =
    takeDirectory .> replaceSingleDot
