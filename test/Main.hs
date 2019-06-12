import qualified Table.SpellResultTests as SpRes
import           Test.Tasty             (defaultMain)

main :: IO ()
main = putStrLn "" >> -- for better visbility when recompiling
  defaultMain SpRes.tests
-- main = do
-- {-
-- different types that impl Testable. Could combine via Existential Quantification to
-- create a heterogenous list of a typeclass, but perhaps there's a better/more supported way?
-- -}
--   mapM_ quickCheck [prop_hitEnemiesMax99, prop_hitAlliesAlwaysSucceeds]
--   mapM_ quickCheck [prop_SpellResolveSemigroupOrder]
