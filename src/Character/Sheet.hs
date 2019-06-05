{-# LANGUAGE OverloadedStrings #-}

module Character.Sheet where

import           Character.Defenses    (Defenses (..))
import qualified Character.Phys        as Phys
import           Character.Resistances (Resistances (..))
import           Data.Text             as T

data Character =
  Character
    { level       :: Float
    , cClass      :: CClass
    , race        :: Race
    , stamina     :: Float
    , strength    :: Float
    , agility     :: Float
    , spirit      :: Float
    , resistances :: Resistances
    , defenses    :: Defenses
    , melee       :: Phys.Stats
    , ranged      :: Phys.Stats
    , spell       :: Spell.Stats
    , guild       :: Maybe T.Text
    }

data Race
  = Human
  | Nightelf
  | Dwarf
  | Gnome
  | Orc
  | Tauren
  | Troll
  | Undead

data CClass
  = Warrior
  | Paladin
  | Shaman
  | Hunter
  | Rogue
  | Druid
  | Priest
  | Mage
  | Warlock
