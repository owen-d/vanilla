{-# LANGUAGE ScopedTypeVariables #-}

module Table.SpellResultTests where

import           Character.Gen            ()
import           Engine.Character         (Character (level))
import           Engine.Character.Sheet   (empty60, spellStats)
import qualified Engine.Character.Spell   as CSpell
import           Engine.Spells.Spell      (Spell (..), beneficial, harmful,
                                           isDirect)
import qualified Engine.Spells.Spell      as Spell
import           Engine.Table.SpellResult (SpellResolve, critChance, hitChance)
import           Spells.Gen               ()
import           Table.Gen                ()
import           Test.QuickCheck.Property (Result (reason), failed, succeeded)
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.HUnit         (testCase, (@?=))
import           Test.Tasty.QuickCheck    (Gen, arbitrary, forAll, suchThat)
import qualified Test.Tasty.QuickCheck    as QC

---------------------------------------------------------------------------
-- tests
---------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "SpellResult" [qcGroup, huGroup]

qcGroup :: TestTree
qcGroup =
  testGroup
    "(tested by QuickCheck)"
    [ prop_hitAlliesAlwaysSucceeds
    , prop_hitEnemiesMax99
    , prop_SpellResolveSemigroupOrder
    , prop_uncrittableSpells
    , prop_critBonusesAdditive
    ]

huGroup :: TestTree
huGroup =
  testGroup "(tested by HUnit)" [tc_hitCalc_0, tc_hitCalc_1, tc_hitCalc_2, tc_hitCalc_3, tc_hitCalc_4, tc_critCalc_0, tc_critCalc_1]


---------------------------------------------------------------------------
-- QuickCheck properties
---------------------------------------------------------------------------

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

prop_hitAlliesAlwaysSucceeds :: TestTree
prop_hitAlliesAlwaysSucceeds =
  QC.testProperty "hit allies always succeeds" $ forAll helpfulSpells hitAlwaysSucceeds

prop_hitEnemiesMax99 :: TestTree
prop_hitEnemiesMax99 =
  QC.testProperty "hit enemies never 100% probability" $ forAll harmfulSpells hitProbLessThan1

prop_SpellResolveSemigroupOrder :: TestTree
prop_SpellResolveSemigroupOrder =
  QC.testProperty "Spell Resolve Semigroup preserves increasing order" test
  where
    test ((a,b) :: (SpellResolve, SpellResolve)) =
      if combined == expected then succeeded else orderFail
        where
          expected = max a b
          combined = a <> b
          orderFail = failed {reason="SpellResolve did not preserve increasing order"}

-- dots/buffs never crit
prop_uncrittableSpells :: TestTree
prop_uncrittableSpells =
  QC.testProperty "non direct spells can't crit" $
  forAll indirectSpells doesntCrit
  where
    indirectSpells = arbitrary `suchThat` notDirect
    notDirect (_, s, _) = not . isDirect . sClass $ s
    doesntCrit ((caster, spell, target) :: ( Character
                                           , Spell Character
                                           , Character)) =
      critChance caster spell target == 0

prop_critBonusesAdditive :: TestTree
prop_critBonusesAdditive =
  QC.testProperty "spell & character crit bonuses are additive" $
  forAll directSpells isAdditive
  where
    directSpells = arbitrary `suchThat` isDirect'
    isDirect' (_, s, _) = isDirect . sClass $ s
    isAdditive ((caster, spell, target) :: ( Character
                                           , Spell Character
                                           , Character)) =
      min hits added == crits
      where
        added = (CSpell.crit . spellStats) caster + critBonus spell
        hits = hitChance caster spell target
        crits = critChance caster spell target

---------------------------------------------------------------------------
-- HUnit Tests
---------------------------------------------------------------------------
tc_hitCalc_0 :: TestTree
tc_hitCalc_0 =
  testCase "sameLevel hit" (actual @?= expected)
  where
    expected = 96/100 :: Float
    actual = hitChance empty60 Spell.empty empty60

tc_hitCalc_1 :: TestTree
tc_hitCalc_1 =
  testCase "one-level-diff hit" (actual @?= expected)
  where
    expected = 95/100 :: Float
    actual = hitChance empty60 Spell.empty empty60{level=61}

tc_hitCalc_2 :: TestTree
tc_hitCalc_2 =
  testCase "two-level-diff hit" (actual @?= expected)
  where
    expected = 94/100 :: Float
    actual = hitChance empty60 Spell.empty empty60{level=62}

tc_hitCalc_3 :: TestTree
tc_hitCalc_3 =
  testCase "bossLevel hit" (actual @?= expected)
  where
    expected = 83/100 :: Float
    actual = hitChance empty60 Spell.empty empty60{level=63}

tc_hitCalc_4 :: TestTree
tc_hitCalc_4 =
  testCase "above mob 1-lvl hit" (actual @?= expected)
  where
    expected = 97/100 :: Float
    actual = hitChance empty60 Spell.empty empty60{level=59}

tc_critCalc_0 :: TestTree
tc_critCalc_0 =
  testCase "crit inherits spellBonuses" (actual @?= expected)
  where
    expected = 0.5
    actual = critChance empty60 Spell.empty{critBonus=expected} empty60

tc_critCalc_1 :: TestTree
tc_critCalc_1 = testCase "crit inherits charBonuses" (actual @?= expected)
  where
    expected = 0.5
    actual =
      critChance
        empty60 {spellStats = mempty {CSpell.crit = expected}}
        Spell.empty
        empty60
