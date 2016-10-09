module Test.Helpers where

import Shikensu (Dictionary, sortByAbsolutePath)
import System.Directory (canonicalizePath)
import Test.Tasty.HUnit (assertEqual)

import qualified Data.List as List


assertDef :: (Show a1, Eq a1) => IO a -> (a -> a1) -> a1 -> IO ()
assertDef definition accessor value =
  (fmap accessor definition) >>= (assertEqual "" value)


rootPath :: IO FilePath
rootPath = canonicalizePath "./"


sort :: Dictionary -> Dictionary
sort = List.sortBy sortByAbsolutePath
