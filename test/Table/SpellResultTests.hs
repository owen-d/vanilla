module Table.SpellResultTests where

import           Character                (Character)
import           Character.Gen            ()
import           Spells.Gen               ()
import           Spells.Spell             (Spell (sClass), SpellClass (..))
import           Table.SpellResult        (hitChance)
import           Test.QuickCheck.Property (Result (..), failed, succeeded)

-- hitchance, critchacne, harmfulcast, modify, expected, maxcrit1, spelldist

{-
hitChance
- if beneficial is always 1
- never be more than 0.99
-
-}

prop_hitEnemiesMax99 :: Character -> Spell Character -> Character -> Result
prop_hitEnemiesMax99 char spell target =
  let harmfulSpell =
        -- map helpful spells into harmful spells for this test
        case sClass spell of
          Helpful x -> Harmful x
          y         -> y
   in if hitChance char spell {sClass = harmfulSpell} target <= 0.99
        then succeeded
        else failed {reason = "cant hit enemies w/ 100% accuracy"}

prop_hitAlliesAlwaysSucceeds :: Character -> Spell Character -> Character -> Result
prop_hitAlliesAlwaysSucceeds  char spell target =
  let helpfulSpell =
        -- map harmful spells into helpful spells for this test
        case sClass spell of
          Harmful  x -> Helpful x
          y          -> y
   in if hitChance char spell {sClass = helpfulSpell} target == 1
        then succeeded
        else failed {reason = "spells on allies always land"}
