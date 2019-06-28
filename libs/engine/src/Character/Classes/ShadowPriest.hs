module Character.Classes.ShadowPriest where

import           Character              (Character)
import           Character.Classes.Spec (Spec (..))
import           EqPoints               (EqPoint (..))
import           Spells.Spell           (SType (..), School (..), Spell (..),
                                         SpellClass (..), empty)

import           Table.SpellResult      (spellDist)


-- | spec assumes something like
-- https://classicdb.ch/?talent#bxhRsZZVMgtcuRt
spec :: Spec
spec =
  Spec
    { attrs = vars
    , mkSpells = const (spellDist spellPrios')
    , buffScale = buffs
    }


spellPrios' :: [Spell Character]
spellPrios' = [mindBlast, mindFlay]

-- | shadow weaving + curse of shadows + shadow form
buffs :: Fractional a => a -> a
buffs y = y * (1 + 0.15 + 0.1 + 0.15) -- shadow weaving + curse of shadows + shadow mastery

vars :: [EqPoint]
vars = [SpellHit, SpellCrit, School Shadow]


mindBlast :: Spell Character
mindBlast =
  empty
    { school = Shadow
    , sClass = Harmful Direct
    , manaCost = 350
    , cooldown = 5.5
    , dmg = 517.5
    , coeff = 1.5 / 3.5
    , castTime = 1.5
    }

mindFlay :: Spell Character
mindFlay =
  empty
    { school = Shadow
    , sClass = Harmful Duration
    , manaCost = 205
    , dmg = 426
    , coeff = 3 / 3.5
    , castTime = 3
    }

