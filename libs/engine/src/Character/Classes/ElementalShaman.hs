module Character.Classes.ElementalShaman where

import           Character              (Character)
import           Character.Classes.Spec (Spec (..))
import           Data.Equivalence.Attr  (Attr (..))
import           Spells.Spell           (SType (..), School (..), Spell (..),
                                         SpellClass (..), empty)

import           Table.SpellResult      (spellDist)

-- | assuming something like
-- https://classic.wowhead.com/talent-calc/shaman/55010105230215--5003231100501
spec :: Spec Attr
spec =
  Spec
    { inputs = attrs
    , mkSpells = const (spellDist spellPrios)
    , buffScale = buffs
    }

-- | Concussion
buffs :: Fractional a => a -> a
buffs y = y * 1.05

spellPrios :: [Spell Character]
spellPrios = [lightningBolt]

-- | spellTalents adds the hit bonus (Nature's Guidance), the crit coefficient (Elemental Fury),
-- the mana cost reduction (Convection + Elemental Focus), the cast time (Lightning Mastery)
-- and the crit bonus (Tidal Mastery + Call of Thunder)
spellTalents :: Spell a -> Spell a
spellTalents s@Spell { critBonus = crit'
                     , hitBonus = hit'
                     , manaCost = mana'
                     , castTime = cast'
                     } =
  s
    { critBonus = crit' + 0.11
    , hitBonus = hit' + 0.03
    , manaCost = mana' * 0.95 * 0.9
    , critCoeff = 2
    , castTime = minCast
    }
  where
    minCast = min (cast' - 1) 1.5

attrs :: [Attr]
attrs = [SpellHit, SpellCrit, School Nature]

lightningBolt :: Spell Character
lightningBolt =
  spellTalents $
  empty
    { school = Nature
    , sClass = Harmful Direct
    , manaCost = 265
    , dmg = 443.5
    , coeff = 3 / 3.5
    , castTime = 3
    }
