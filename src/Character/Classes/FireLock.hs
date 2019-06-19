module Character.Classes.FireLock where

import           Character         (Character)
import           Spells.Spell      (SType (..), School (..), Spell (..),
                                    SpellClass (..), empty)
import           Table.SpellResult (cast, expectedDmg, maxCritN)

-- spells assume SM/Ruin pts in suppression

raidbuffs :: Fractional a => a -> a
raidbuffs y = y * (1 + 0.15 + 0.1 + 0.15) -- imp scorch + curse of elements + imp sac

spellPrios :: [Spell Character]
spellPrios = [curseOfDoom, searingPain]

-- 504 healing w/ imp lifetap 20%
lifeTap :: Spell a
lifeTap = empty {school = Shadow, sClass = Helpful Buff, coeff = 0.8}

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

searingPain :: Spell Character
searingPain =
  empty
    { school = Fire
    , sClass = Harmful Direct
    , manaCost = 168
    , dmg = 222.5
    , coeff = 1.5 / 3.5
    , critBonus = 0.15
    , critCoeff = 2
    , castTime = 1.5
    }

improvedSbMod :: Spell Character -> Character -> Character -> Spell Character
improvedSbMod spell@ Spell{critFlatBonuses=cfbs} caster target =
  spell{critFlatBonuses=cfbs ++ [bonus]}
  where
    bonusCoeff = 0.2
    charges = 4 -- charges of imp sb yielding 20% dmg each
    baseResult = cast spell caster target -- get damage distribution
    bonus = bonusCoeff * (expectedDmg $ maxCritN baseResult charges 1)


