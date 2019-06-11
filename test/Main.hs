import           Table.SpellResultTests
import           Test.QuickCheck        (quickCheck)

main :: IO ()
main = quickCheck prop_hitEnemiesMax99
