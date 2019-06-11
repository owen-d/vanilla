module Character.Gen where

import           Character.Defenses        (Defenses)
import qualified Character.Phys            as Phys
import           Character.Resistances     (Resistances)
import           Character.Sheet           (CClass, Character, Race)
import qualified Character.Spell           as CSpell
import           Generic.Random            (genericArbitrary, uniform)
import           Test.QuickCheck           (Arbitrary (..))
import           Test.QuickCheck.Instances ()

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
