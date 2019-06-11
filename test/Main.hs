import           Table.SpellResultTests
import           Test.QuickCheck        (quickCheck)

main :: IO ()
main = mapM_ quickCheck [prop_hitEnemiesMax99, prop_hitAlliesAlwaysSucceeds]
