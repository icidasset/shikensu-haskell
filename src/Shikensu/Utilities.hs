module Shikensu.Utilities where

import Data.Maybe (fromMaybe)
import Data.Tuple (fst)
import Flow
import Shikensu.Types
import System.FilePath

import qualified Data.List as List (map, stripPrefix)
import qualified System.FilePath.Glob as Glob


{-| "Clean up" a path.

`/directory/./nested/` -> `directory/nested`
`./` -> ``
`.` -> ``

-}
cleanPath :: FilePath -> FilePath
cleanPath = normalise .> dropTrailingPathSeparator .> dropDrive .> replaceSingleDot


{-| Get the common directory from a Pattern
-}
commonDirectory :: Pattern -> FilePath
commonDirectory pattern = (Glob.compile .> Glob.commonDirectory .> fst) pattern


{-| Compile a list of `Pattern`s to `Glob.Pattern`s.
-}
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


{-| If the path is a single dot, return an empty string.
Otherwise return the path.
-}
replaceSingleDot :: String -> String
replaceSingleDot path =
  if path == "." then "" else path


{-| Strip prefix
-}
stripPrefix :: String -> String -> String
stripPrefix prefix target = fromMaybe target (List.stripPrefix prefix target)
