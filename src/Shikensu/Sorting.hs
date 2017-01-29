{-| Sorting functions.

-}
module Shikensu.Sorting
    ( sortByAbsolutePath
    ) where

import Shikensu.Types (Definition, absolutePath)


{-| Sort by absolutePath.

> Data.List.sortBy Shikensu.sortByAbsolutePath dictionary

-}
sortByAbsolutePath :: Definition -> Definition -> Ordering
sortByAbsolutePath defA defB =
    compare (absolutePath defA) (absolutePath defB)
