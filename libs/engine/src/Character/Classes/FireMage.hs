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

-- | spellTalents adds the hit % bonus from Elemental Precision,
-- the crit bonus from Critical Mass, and the crit coefficient from Ignite
spellTalents :: Spell a -> Spell a
spellTalents s@Spell {hitBonus = hit', critBonus = crit', critCoeff=coeff'} =
  s {hitBonus = hit' + 0.06, critBonus = crit' + 0.06, critCoeff=coeff' * 1.4}

spellPrios :: [Spell Character]
spellPrios = [fireball]

attrs :: [Attr]
attrs = [SpellHit, SpellCrit, School Fire]

fireball :: Spell Character
fireball =
  spellTalents $
  empty
    { school = Fire
    , sClass = Harmful Direct
    , manaCost = 290
    , dmg = 638.5
    , coeff = 3.5 / 3.5
    , castTime = 3
    }
