module Test.Helpers where

import Shikensu (sortByAbsolutePath)
import Shikensu.Types (Dictionary)
import System.Directory (canonicalizePath)
import System.FilePath (combine)
import Test.Tasty.HUnit (Assertion, assertEqual)

import qualified Data.List as List


assertEq :: (Eq a, Show a) => a -> a -> Assertion
assertEq = assertEqual ""


ioErrorHandler :: IOError -> IO ()
ioErrorHandler _ = putStrLn ""


rootPath :: IO FilePath
rootPath = canonicalizePath "./"


sort :: Dictionary -> Dictionary
sort = List.sortBy sortByAbsolutePath


testsPath :: IO FilePath
testsPath = fmap ((flip combine) "tests") rootPath
