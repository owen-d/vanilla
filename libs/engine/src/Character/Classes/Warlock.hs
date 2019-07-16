{-# LANGUAGE FlexibleContexts #-}

module Character.Classes.Warlock where

import           Character              (Character (spellStats))
import           Character.Classes.Spec (Spec (..))
import           Character.Spell        (Stats (shadow))
import           Data.Equivalence.Attr  (Attr (..))
import           Data.Function          (fix)
import           Dist                   (Dist (..), rounds)
import           Spells.Spell           (SType (..), School (..), Spell (..),
                                         SpellClass (..), empty, mkModifiers)
import           Table.SpellResult      (cast, expectedDmg, isCrit,
                                         spellDistWithReserved)


-- spells assume SM/Ruin pts in suppression
spec :: Spec Attr
spec =
  Spec
    { inputs = attrs
    , mkSpells = \c -> spellDist $ (shadow . spellStats) c
    , buffScale = buffs
    }

buffs :: Fractional a => a -> a
buffs y = y * (1 + 0.15 + 0.1 + 0.1) -- shadow weaving + curse of shadows + shadow mastery

spellPrios :: [Spell Character]
spellPrios = [curseOfDoom, shadowBolt]

attrs :: [Attr]
attrs = [SpellHit, SpellCrit, School Shadow]

-- fix :: (a -> a) -> a
-- fix f = let {x = f x} in x

-- -- examples
-- z y = fix f y
--   where
--     f x =
--       if x <= 0 then x else f (x-1)

-- z x =
--   fix
--     (\f b ->
--        if b <= 0
--          then b
--          else f (b - 1))
--   x

-- (a -> a)
-- subsituting a = (b -> c)
-- ((b -> c) -> (b -> c)) -> (b -> c)
-- simplify: ((b -> c) -> b -> c) -> b -> c

{-
spellDist calculates the fixed point of a spell rotation which includes lifetaps.
The idea is that for a given spell distribution, you can calculate how many lifetaps are required
to break even. Adding those to the distribution alters it, though. Thus, we use the y-combinator
to calculat the fixed point, iteratively adjusting the distribution until we reach an acceptable
threshold of accuracy.
-}
spellDist :: Float -> Dist (Spell Character)
spellDist spellDmg =
  fix approximate initialDist
  where
    initialDist = ((spellDistWithReserved spellPrios 0), 0)
    approximate recur (Dist xs, reserved) =
     let totalCost = sum $ map (\(x, p) -> p * manaCost x) xs
         perTap = 504 + 0.8 * spellDmg -- mana gained per lifetap
         lifetaps cost = cost / perTap -- # of lifetaps required to gain x mana
         tapsIn t = t / castTime lifeTap -- # of life taps in a period t
         manaReserved = tapsIn reserved * perTap -- mana gained from lifetapping w/ reserved time
         reservedDiff = lifetaps (manaReserved - totalCost) * castTime lifeTap -- num lifetaps to add/subtract to hit new distribution
         reserved' = reserved - reservedDiff -- how much to reserve next attempt
         finished = abs reservedDiff < 0.001 -- arbitrary finished threshold
     in if finished
        then Dist $ xs ++ [(lifeTap, tapsIn reserved)] -- add lifetaps into reserved space
        else recur (spellDistWithReserved spellPrios reserved', reserved')

-- 504 healing w/ imp lifetap 20%
-- hack: classify as Harmful Buff b/c we don't currently have a way to cast helpful spells
lifeTap :: Spell a
lifeTap = empty {school = Shadow, sClass = Harmful Buff, coeff = 0.8}

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
    , critBonus = 0.05 -- Devastation
    , critCoeff = 2 -- Ruin
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
    bonus = bonusCoeff * (expectedDmg $ max1Crit charges)
    max1Crit n = trim <$> rounds n baseResult
      where
        trim [] = []
        trim (x:xs)
          | isCrit x = [x]
          | otherwise = x : trim xs
