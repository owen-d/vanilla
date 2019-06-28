module Character.Classes.BalanceDruid where

import           Character              (Character)
import           Character.Classes.Spec (Spec (..))
import           Dist                   (coalesceWith)
import           EqPoints               (EqPoint (..))
import           Spells.Spell           (SType (..), School (..), Spell (..),
                                         SpellClass (..), empty, mkModifiers)
import           Table.SpellResult      (cast, spellDist)
import qualified Table.SpellResult      as SpRes

-- | assuming something like
-- https://classic.wowhead.com/talent-calc/druid/5110503002551351--0505031
balanceDruid :: Spec
balanceDruid =
  Spec
    { attrs = vars
    , mkSpells = const (spellDist spellPrios)
    , buffScale = raidbuffs
    }

-- | curse of shadows + moonfury
raidbuffs :: Fractional a => a -> a
raidbuffs y = y * (1 + 0.1 + 0.1)

spellPrios :: [Spell Character]
spellPrios = [starfire]

vars :: [EqPoint]
vars = [SpellHit, SpellCrit, School Arcane]

starfire :: Spell Character
starfire =
  empty
    { school = Arcane
    , sClass = Harmful Direct
    , manaCost = 340
    , dmg = 540.5 -- weird, way lower than fireball (same lvl/cast time)
    , coeff = 3.5 / 3.5
    , critBonus = 0.15 -- moonkin aura (applied to an entire caster group but credited here to moonkin)
    , critCoeff = 2 -- Vengeance
    , castTime = 3
    , modifiers = mkModifiers [naturesGrace]
    }

-- | naturesGrace credits the spell with 0.5s of damage from a subsequent cast
naturesGrace :: Spell Character -> Character -> Character -> Spell Character
naturesGrace spell@ Spell{critFlatBonuses=cfbs, castTime=cTime} caster target =
  spell{critFlatBonuses=cfbs ++ [bonus]}
  where
    bonus = 0.5 / cTime * outcome
    outcome = coalesceWith aggregate 0 baseResult
    aggregate p x acc = p * (SpRes.dmg x) + acc
    baseResult = cast spell caster target -- get damage distribution