module Spells.Gen where

import           Generic.Random  (genericArbitrary, uniform)
import           Spells.Spell
import           Test.QuickCheck (Arbitrary (..))

instance Arbitrary School where
  arbitrary = genericArbitrary uniform

instance Arbitrary SType where
  arbitrary = genericArbitrary uniform

instance Arbitrary SpellClass where
  arbitrary = genericArbitrary uniform

instance Arbitrary (Spell a) where
  arbitrary =
    Spell <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    arbitrary <*>
    return [] -- hardcodes modifiers to empty list for now until i grok CoArbitrary
