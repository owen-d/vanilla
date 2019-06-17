module Character.Classes.Warlock where

import           Character         (Character)
import           Spells.Spell      (SType (..), School (..), Spell (..),
                                    SpellClass (..), empty, mkModifiers)
import           Table.SpellResult (cast, expectedDmg, maxCritN)

-- spells assume DS/Ruin w/ 2 pts in suppression

-- shadow weaving + curse of shadows
raidbuffs :: Fractional a => a -> a
raidbuffs y = y * 1.15 * 1.1

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
    , cooldown = 60
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
    , modifiers = mkModifiers [improvedSbMod]
    }

corruption :: Spell Character
corruption =
  empty
    { school = Shadow
    , sClass = Harmful Duration
    , manaCost = 340
    , dmg = 822
    , coeff = 1.2
    , duration = 18
    }

improvedSbMod :: Spell Character -> Character -> Character -> Spell Character
improvedSbMod spell@ Spell{critFlatBonuses=cfbs} caster target =
  spell{critFlatBonuses=cfbs ++ [bonus]}
  where
    bonusCoeff = 0.2
    charges = 4 -- charges of imp sb yielding 20% dmg each
    baseResult = cast spell caster target -- get damage distribution
    bonus = bonusCoeff * (expectedDmg $ maxCritN baseResult charges 1)


