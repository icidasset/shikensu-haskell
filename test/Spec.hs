import System.Directory (getCurrentDirectory)
import qualified Shikensu


{-| TODO

Write tests for multiple cases:

1. Pattern with "static" directory (e.g. "src/**/*.hs")
2. Pattern without "static" directory (e.g. "**/*.hs")
3. Pattern with leading dot-slash (e.g. "./src/**/*.hs")
4. ...

-}


main :: IO (Shikensu.Dictionary)
main =
  getCurrentDirectory >>= Shikensu.list ["src/**/*.hs"]
