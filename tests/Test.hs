import Test.Contrib
import Test.Shikensu
import Test.Tasty


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [shikensuTests, contribTests]
