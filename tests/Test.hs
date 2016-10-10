import Control.Exception (catch)
import System.Directory (removeDirectoryRecursive)
import System.FilePath (combine)
import Test.Contrib
import Test.Helpers (ioErrorHandler, rootPath)
import Test.Shikensu
import Test.Tasty


main :: IO ()
main =
  rootPath
    >>= \r -> catch
      (removeDirectoryRecursive (combine r "tests/build"))
      (ioErrorHandler)
    >> defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [shikensuTests, contribTests]
