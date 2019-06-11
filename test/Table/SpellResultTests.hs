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
prop_hitEnemiesMax99 char spell target=
  case sClass spell of
    Helpful _ -> succeeded
    Harmful _ ->
      if hitChance char spell target <= 99
        then succeeded
        else failed {reason = "cant hit enemies w/ 100% accuracy"}
