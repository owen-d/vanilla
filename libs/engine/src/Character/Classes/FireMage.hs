module Character.Classes.FireMage where

import           Character              (Character)
import           Character.Classes.Spec (Spec (..))
import           Data.Equivalence.Attr  (Attr (..))
import           Spells.Spell           (SType (..), School (..), Spell (..),
                                         SpellClass (..), empty)
import           Table.SpellResult      (spellDist)

spec :: Spec Attr
spec =
  Spec
    { inputs = attrs
    , mkSpells = const (spellDist spellPrios)
    , buffScale = buffs
    }


-- imp scorch * curse of elements
buffs :: Fractional a => a -> a
buffs y = y * (1 + 0.15 + 0.1 + 0.1) -- imp scorch + curse of elements + fire power

spellPrios :: [Spell Character]
spellPrios = [fireball]

attrs :: [Attr]
attrs = [SpellHit, SpellCrit, School Fire]

fireball :: Spell Character
fireball =
  empty
    { school = Fire
    , sClass = Harmful Direct
    , manaCost = 290
    , dmg = 638.5
    , coeff = 3.5 / 3.5
    , hitBonus = 0.06
    , critBonus = 0.06
    , critCoeff = 2.1 -- ignite
    , castTime = 3
    }
