module Character.Classes.ArcaneMage where

import           Character                   (Character)
import           Character.Classes.FrostMage (buffs, spellPrios, vars)
import           Character.Classes.Spec      (Spec (..))
import           Spells.Spell                (Spell (..))
import           Table.SpellResult           (spellDist)

arcaneMage :: Spec
arcaneMage =
  Spec
  { attrs = vars
  , mkSpells = const (spellDist spellPrios')
  , buffScale = buffScale'
  }

buffs' :: Float
buffs' = buffs + 0.03 -- arcane instability percent dmg

spellPrios' :: [Spell Character]
spellPrios' = map f spellPrios
  where
    f spell = spell{critBonus = critBonus spell + 0.03 } -- arcane instability crit

buffScale' :: Float -> Float
buffScale' y = uptime *  y * (buffs' + 0.3)
  + downtime * y * (buffs')
  where
    uptime = 15/(60 * 3) -- arcane power uptime as a fraction of cooldown
    downtime = 1-uptime
