import           Table.SpellResultTests
import           Test.QuickCheck        (quickCheck)

main :: IO ()
main = do
{-
different types that impl Testable. Could combine via Existential Quantification to
create a heterogenous list of a typeclass, but perhaps there's a better/more supported way?
-}
  mapM_ quickCheck [prop_hitEnemiesMax99, prop_hitAlliesAlwaysSucceeds]
  mapM_ quickCheck [prop_SpellResolveSemigroupOrder]
