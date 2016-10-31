{-# LANGUAGE OverloadedStrings #-}
module Shikensu.Sorting
  ( sortByAbsolutePath
  ) where


{-| Sorting functions.

Example:

        Data.List.sortBy Shikensu.sortByAbsolutePath dictionary

-}

import Shikensu.Types (Definition, absolutePath)



sortByAbsolutePath :: Definition -> Definition -> Ordering
sortByAbsolutePath defA defB =
  compare (absolutePath defA) (absolutePath defB)
