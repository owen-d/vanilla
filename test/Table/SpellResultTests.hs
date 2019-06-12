{-# LANGUAGE ScopedTypeVariables #-}

module Table.SpellResultTests where

import           Character                (Character)
import           Character.Gen            ()
import           Spells.Gen               ()
import           Spells.Spell             (Spell, beneficial, harmful)
import           Table.Gen                ()
import           Table.SpellResult        (SpellResolve, hitChance)
import           Test.QuickCheck.Property (Result (..), failed, succeeded)
import           Test.Tasty               (testGroup)
import           Test.Tasty.QuickCheck    (Gen, arbitrary, forAll, suchThat)
import qualified Test.Tasty.QuickCheck    as QC

tests = testGroup "SpellResult" [qcGroup]
qcGroup =
  testGroup
    "(tested by QuickCheck)"
    [prop_hitAlliesAlwaysSucceeds, prop_hitEnemiesMax99, prop_SpellResolveSemigroupOrder]

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
  QC.testProperty "hit allies always succeeds" $ forAll helpfulSpells hitAlwaysSucceeds

prop_hitEnemiesMax99 =
  QC.testProperty "hit enemies never 100% probability" $ forAll harmfulSpells hitProbLessThan1

prop_SpellResolveSemigroupOrder =
  QC.testProperty "Spell Resolve Semigroup preserves increasing order" test
  where
    test ((a,b) :: (SpellResolve, SpellResolve)) =
      if combined == expected then succeeded else orderFail
        where
          expected = max a b
          combined = a <> b
          orderFail = failed {reason="SpellResolve did not preserve increasing order"}
