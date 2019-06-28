module Character.Classes.FrostMage where

import           Character              (Character)
import           Character.Classes.Spec (Spec (..))
import           EqPoints               (EqPoint (..))
import           Spells.Spell           (SType (..), School (..), Spell (..),
                                         SpellClass (..), empty)
import           Table.SpellResult      (spellDist)

spec :: Spec
spec =
  Spec
    { attrs = vars
    , mkSpells = const (spellDist spellPrios)
    , buffScale = \y -> y * buffs
    }


buffs :: Float
buffs = (1 + 0.06 + 0.1) -- ice shards * curse of shadows

vars :: [EqPoint]
vars = [SpellHit, SpellCrit, School Frost]

spellPrios :: [Spell Character]
spellPrios = [frostBolt]

frostBolt :: Spell Character
frostBolt =
  empty
    { school = Frost
    , sClass = Harmful Direct
    , manaCost = 290
    , dmg = 535.5
    , coeff = 3 / 3.5
    , hitBonus = 0.06
    , critBonus = 0.10 -- winter's chill
    , critCoeff = 2
    , castTime = 2.5
    }
