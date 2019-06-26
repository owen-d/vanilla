module Engine.Character.Classes.FireMage where

import           Engine.Character    (Character)
import           Engine.Spells.Spell (SType (..), School (..), Spell (..),
                                      SpellClass (..), empty)

-- imp scorch * curse of elements
raidbuffs :: Fractional a => a -> a
raidbuffs y = y * (1 + 0.15 + 0.1 + 0.1) -- imp scorch + curse of elements + fire power

spellPrios :: [Spell Character]
spellPrios = [fireball]

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
