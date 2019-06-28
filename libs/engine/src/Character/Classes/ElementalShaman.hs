module Character.Classes.ElementalShaman where

import           Character              (Character)
import           Character.Classes.Spec (Spec (..))
import           EqPoints               (EqPoint (..))
import           Spells.Spell           (SType (..), School (..), Spell (..),
                                         SpellClass (..), empty)

import           Table.SpellResult      (spellDist)

-- | assuming something like
-- https://classic.wowhead.com/talent-calc/shaman/55010105230215--5003231100501
spec :: Spec
spec =
  Spec
    { attrs = vars
    , mkSpells = const (spellDist spellPrios)
    , buffScale = buffs
    }

-- | elemental shamans do not have any benefitting % based buffs
buffs :: Fractional a => a -> a
buffs y = y

spellPrios :: [Spell Character]
spellPrios = [lightningBolt]

vars :: [EqPoint]
vars = [SpellHit, SpellCrit, School Nature]

lightningBolt :: Spell Character
lightningBolt =
  empty
    { school = Nature
    , sClass = Harmful Direct
    , manaCost = 265
    , dmg = 443.5
    , coeff = 3 / 3.5
    , critBonus = 0.11
    , critCoeff = 2
    , castTime = 2
    }
