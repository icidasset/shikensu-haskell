module Shikensu.Contrib.IO where

import Shikensu.Types (Dictionary)
import qualified Shikensu.Contrib.Pure as Pure


read :: IO Dictionary -> IO Dictionary
read = fmap Pure.read
