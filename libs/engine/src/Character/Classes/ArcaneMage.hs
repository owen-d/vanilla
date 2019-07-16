module Character.Classes.ArcaneMage
  ( module Character.Classes.ArcaneMage
  , attrs
  ) where

import           Character                   (Character)
import           Character.Classes.FrostMage (attrs, buffs, spellPrios)
import           Character.Classes.Spec      (Spec (..))
import           Data.Equivalence.Attr       (Attr (..))
import           Spells.Spell                (Spell (..))
import           Table.SpellResult           (spellDist)

spec :: Spec Attr
spec =
  Spec
  { inputs = attrs
  , mkSpells = const (spellDist spellPrios')
  , buffScale = buffScale'
  }

buffs' :: Float
buffs' = buffs + 0.03 -- arcane instability percent dmg

-- | spellTalents adds the crit % bonus from Arcane Instability
spellTalents :: Spell a -> Spell a
spellTalents s@Spell {critBonus = crit'} = s {critBonus = crit' + 0.03}

spellPrios' :: [Spell Character]
spellPrios' = map spellTalents spellPrios

buffScale' :: Float -> Float
buffScale' y = sum [uptime * y * (buffs' + 0.3), downtime * y * buffs']
  where
    uptime = 15 / (60 * 3) -- arcane power uptime probability via fraction of cooldown
    downtime = 1 - uptime
