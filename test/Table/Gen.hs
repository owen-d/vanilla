module Table.Gen where

import           Generic.Random    (genericArbitrary, uniform)
import           Table.SpellResult
import           Test.Tasty.QuickCheck   (Arbitrary (..))

instance Arbitrary SpellResult where
  arbitrary = genericArbitrary uniform

instance Arbitrary SpellResolve where
  arbitrary = genericArbitrary uniform
