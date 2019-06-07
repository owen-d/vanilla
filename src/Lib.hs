module Lib
    ( someFunc
    ) where

import           Character                 (CClass (..), Character (..),
                                            Race (..))
import qualified Character                 as CChar
import qualified Character.Classes.Warlock as Wlock
import           Character.Spell           (Stats (Stats), spellPower)
import qualified Character.Spell           as Csp
import           Prob                      (Prob (..), run)
import           Spells.Spell              (SType (..), School (..),
                                            Spell (Spell), SpellClass (..))
import qualified Spells.Spell              as Spell
import           Table.SpellResult         (SpellResult (SpellResult), cast,
                                            expected)
import qualified Table.SpellResult         as SpRes
import           Text.Printf               (printf)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

hero = Character
 {level=60
 , cClass = Warlock
 , race = Gnome
 , stamina = 100
 , strength = 100
 , agility = 100
 , spirit = 100
 , resistances=mempty
 , defenses=mempty
 , meleeStats=mempty
 , rangedStats=mempty
 , spellStats=mempty{Csp.shadow=120, Csp.crit=0.02, Csp.hit=0.04}
 , guild=Nothing}

sameLevelEnemy =
  Character
    { level = 60
    , cClass = Warrior
    , race = Human
    , stamina = 100
    , strength = 100
    , agility = 100
    , spirit = 100
    , resistances = mempty
    , defenses = mempty
    , meleeStats = mempty
    , rangedStats = mempty
    , spellStats = mempty
    , guild = Nothing
    }

boss =
  Character
    { level = 63
    , cClass = Warrior
    , race = Human
    , stamina = 100
    , strength = 100
    , agility = 100
    , spirit = 100
    , resistances = mempty
    , defenses = mempty
    , meleeStats = mempty
    , rangedStats = mempty
    , spellStats = mempty
    , guild = Nothing
    }

res = cast Wlock.shadowBolt hero boss

res' = cast Wlock.shadowBolt{Spell.modifiers=[], Spell.critFlatBonuses=[415.03458]} hero boss

-- equivalent, yay!
