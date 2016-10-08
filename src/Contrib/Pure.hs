module Shikensu.Contrib.Pure where

import Shikensu (Dictionary)


{-| Read
-}
read :: Dictionary -> Dictionary
read dict =
  dict { content = Just "Yes" }
