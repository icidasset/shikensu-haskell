import Debug.Trace
import System.Directory (getCurrentDirectory)
import qualified Shikensu


debug = flip trace


main :: IO (Shikensu.Dictionary)
main =
  getCurrentDirectory >>= Shikensu.list ["**/*.hs"] `debug` "list"
