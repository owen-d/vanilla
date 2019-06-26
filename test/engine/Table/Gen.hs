module Table.Gen where

import           Engine.Table.SpellResult
import           Generic.Random           (genericArbitrary, uniform)
import           Test.Tasty.QuickCheck    (Arbitrary (..))

instance Arbitrary SpellResult where
  arbitrary = genericArbitrary uniform

instance Arbitrary SpellResolve where
  arbitrary = genericArbitrary uniform
