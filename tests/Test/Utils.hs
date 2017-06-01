module Test.Utils where

import Prelude hiding (print)
import Shikensu.Types (Dictionary)
import Text.Show.Pretty (pPrint)

import qualified Data.List as List (head)


print :: Show a => a -> IO ()
print = pPrint


{- Print the first thing from a `Dictionary` -}
printDF :: IO Dictionary -> IO ()
printDF dict =
    fmap List.head dict >>= print
