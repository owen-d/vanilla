module Character.Classes.FrostMage where

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
    , buffScale = \y -> y * buffs
    }


buffs :: Float
buffs = (1 + 0.06 + 0.1) -- ice shards * curse of shadows

attrs :: [Attr]
attrs = [SpellHit, SpellCrit, School Frost]

spellPrios :: [Spell Character]
spellPrios = [frostBolt]

-- | spellTalents adds the hit % bonus from Elemental Precision,
-- the crit bonus from Winter's Chill, and the crit coefficient from Ice Shards
spellTalents :: Spell a -> Spell a
spellTalents s@Spell {hitBonus = hit', critBonus = crit'} =
  s {hitBonus = hit' + 0.06, critBonus = crit' + 0.1, critCoeff=2}

frostBolt :: Spell Character
frostBolt =
  spellTalents $
  empty
    { school = Frost
    , sClass = Harmful Direct
    , manaCost = 290
    , dmg = 535.5
    , coeff = 3 / 3.5
    , castTime = 2.5
    }
