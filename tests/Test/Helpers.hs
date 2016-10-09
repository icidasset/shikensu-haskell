module Test.Helpers where

import Shikensu (Dictionary, sortByAbsolutePath)
import System.Directory (canonicalizePath)
import System.FilePath (combine)
import Test.Tasty.HUnit (Assertion, assertEqual)

import qualified Data.List as List


assertDef :: (Show b, Eq b) => IO a -> (a -> b) -> b -> IO ()
assertDef definition accessor value =
  (fmap accessor definition) >>= (assertEq value)


assertEq :: (Eq a, Show a) => a -> a -> Assertion
assertEq = assertEqual ""


rootPath :: IO FilePath
rootPath = canonicalizePath "./"


sort :: Dictionary -> Dictionary
sort = List.sortBy sortByAbsolutePath


testsPath :: IO FilePath
testsPath = fmap ((flip combine) "tests") rootPath
