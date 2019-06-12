module Table.SpellResultTests where

import           Character                (Character)
import           Character.Gen            ()
import           Spells.Gen               ()
import           Spells.Spell             (Spell, beneficial, harmful)
import           Table.Gen                ()
import           Table.SpellResult        (SpellResolve, hitChance)
import           Test.QuickCheck          (Gen, arbitrary, forAll, suchThat)
import           Test.QuickCheck.Property (Result (..), failed, succeeded)

-- hitchance, critchacne, harmfulcast, modify, expected, maxcrit1, spelldist

{-
hitChance
- if beneficial is always 1
- never be more than 0.99
-
-}

helpfulSpells :: Gen (Character, Spell Character, Character)
helpfulSpells =
  arbitrary `suchThat` isBeneficial
  where
    isBeneficial = \(_,s,_) -> beneficial s

harmfulSpells :: Gen (Character, Spell Character, Character)
harmfulSpells =
  arbitrary `suchThat` isBeneficial
  where
    isBeneficial = \(_,s,_) -> harmful s

hitAlwaysSucceeds :: (Character,Spell Character,Character) -> Result
hitAlwaysSucceeds  (char, spell, target) =
    if hitChance char spell target == 1
      then succeeded
      else failed {reason = "spells on allies always land"}

hitProbLessThan1 :: (Character,Spell Character,Character) -> Result
hitProbLessThan1 (char, spell, target) =
  if hitChance char spell target <= 0.99
  then succeeded
  else failed {reason = "cant hit enemies w/ 100% accuracy"}

prop_hitAlliesAlwaysSucceeds =
  forAll helpfulSpells hitAlwaysSucceeds

prop_hitEnemiesMax99 =
  forAll harmfulSpells hitProbLessThan1

prop_SpellResolveSemigroupOrder :: SpellResolve -> SpellResolve -> Result
prop_SpellResolveSemigroupOrder a b
  | a >= b = if combined == a then succeeded else orderFail
  | otherwise = if combined == b then succeeded else orderFail
    where
      combined = a <> b
      orderFail = failed {reason="SpellResolve did not preserve increasing order"}
