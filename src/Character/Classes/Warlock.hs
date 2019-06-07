module Character.Classes.Warlock where

import           Character           (Character)
import qualified Control.Applicative as Applicative
import           Prob                (Prob (..))
import           Spells.Spell        (SType (..), School (..), Spell (..),
                                      SpellClass (..), empty)
import           Table.SpellResult   (SpellResult (SpellResult), cast, expected)
import qualified Table.SpellResult   as SpRes

-- spells assume DS/Ruin w/ 2 pts in suppression

lifeTap :: Spell a
lifeTap = empty {school = Shadow, sClass = Helpful Buff}

curseOfDoom :: Spell a
curseOfDoom =
  empty
    { school = Shadow
    , sClass = Harmful Duration
    , hitBonus = 0.04
    , dmg = 3200
    , coeff = 2
    , duration = 60
    }

shadowBolt :: Spell Character
shadowBolt =
  empty
    { school = Shadow
    , sClass = Harmful Direct
    , dmg = 510.5
    , coeff = 3 / 3.5
    , critBonus = 0.05
    , critCoeff = 2
    , castTime = 2.5
    , modifiers = [improvedSbMod]
    }

improvedSbMod :: Spell Character -> Character -> Character -> Spell Character
improvedSbMod spell@ Spell{critFlatBonuses=cfbs} caster target =
  spell{critFlatBonuses=cfbs ++ [bonus]}
  where
    bonusCoeff = 0.2
    charges = 4 -- charges of imp sb yielding 20% dmg each
    baseResult = cast spell caster target -- get damage distribution
    bonus = bonusCoeff * (expected $ maxCritOnce baseResult charges)


-- maxCritOnce takes SpellResult probability distribution and returns the probability distribution
-- that includes up to one crit over the specified number of rounds
maxCritOnce :: Prob SpellResult -> Int -> Prob SpellResult
maxCritOnce _ 0 = Applicative.empty
maxCritOnce dRound n = runRound dRound (n - 1)
  where
    runRound d 0 = d
    runRound d n = do
      res@ SpellResult{SpRes.dmg=dmg, SpRes.resolved=t} <- d
      SpellResult{SpRes.dmg=dmg', SpRes.resolved=t'} <- dRound
      if SpRes.Crit == t
        then return res
        else
          let d' = return SpellResult{SpRes.dmg = dmg + dmg', SpRes.resolved=t <> t'}
          in runRound d' (n - 1)
