import           Test.Tasty (defaultMain)
import           Test.Tasty (TestTree, testGroup)

main :: IO ()
main = putStrLn "" >> -- for better visbility when recompiling
  defaultMain tests

tests :: TestTree
tests = testGroup "SpellResult" []
