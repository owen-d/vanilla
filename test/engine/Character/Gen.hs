module Character.Gen where

import           Engine.Character.Defenses    (Defenses)
import qualified Engine.Character.Phys        as Phys
import           Engine.Character.Resistances (Resistances)
import           Engine.Character.Sheet       (CClass, Character, Race)
import qualified Engine.Character.Spell       as CSpell
import           Generic.Random               (genericArbitrary, uniform)
import           Test.QuickCheck.Instances    ()
import           Test.Tasty.QuickCheck        (Arbitrary (..))

instance Arbitrary CSpell.Stats where
  arbitrary = genericArbitrary uniform

instance Arbitrary Phys.Stats where
  arbitrary = genericArbitrary uniform

instance Arbitrary Resistances where
  arbitrary = genericArbitrary uniform

instance Arbitrary Defenses where
  arbitrary = genericArbitrary uniform

instance Arbitrary Race where
  arbitrary = genericArbitrary uniform

instance Arbitrary CClass where
  arbitrary = genericArbitrary uniform

instance Arbitrary Character where
  arbitrary = genericArbitrary uniform
