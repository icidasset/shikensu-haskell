module Shikensu.Contrib.IO where

import Shikensu.Types (Dictionary)
import qualified Shikensu.Contrib.Pure as Pure


read :: IO Dictionary -> IO Dictionary
read = fmap Pure.read


write :: String -> IO Dictionary -> IO Dictionary
write x y = fmap (Pure.write x) y
