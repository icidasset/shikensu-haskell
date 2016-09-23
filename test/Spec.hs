import System.Directory (getCurrentDirectory)
import qualified Shikensu


main :: IO (Shikensu.Dictionary)
main =
  getCurrentDirectory >>= Shikensu.list ["src/**/*.hs"]
