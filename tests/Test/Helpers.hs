module Test.Helpers where

import Shikensu (sortByAbsolutePath)
import Shikensu.Types (Dictionary)
import System.Directory (canonicalizePath)
import System.FilePath (combine)
import Test.Tasty.HUnit (Assertion, assertEqual)

import qualified Data.List as List


rmap :: Functor f => f a -> (a -> b) -> f b
rmap = flip fmap


assertEq :: (Eq a, Show a) => a -> a -> Assertion
assertEq = assertEqual ""


rootPath :: IO FilePath
rootPath = canonicalizePath "./"


sort :: Dictionary -> Dictionary
sort = List.sortBy sortByAbsolutePath


testsPath :: IO FilePath
testsPath = fmap ((flip combine) "tests") rootPath
