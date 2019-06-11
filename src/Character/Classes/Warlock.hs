module Character.Classes.Warlock where

import           Character         (Character)
import           Dist              (Dist (..))
import           Spells.Spell      (SType (..), School (..), Spell (..),
                                    SpellClass (..), empty)
import           Table.SpellResult (SpellResult (SpellResult), cast, expected,
                                    maxCrit1)

-- spells assume DS/Ruin w/ 2 pts in suppression

lifeTap :: Spell a
lifeTap = empty {school = Shadow, sClass = Helpful Buff}

curseOfDoom :: Spell a
curseOfDoom =
  empty
    { school = Shadow
    , sClass = Harmful Duration
    , manaCost = 300
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
    , manaCost = 380
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
    bonus = bonusCoeff * (expected $ maxCrit1 baseResult charges)


