{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Character.Sheet where

import           Character.Defenses    (Defenses (..))
import qualified Character.Phys        as Phys
import           Character.Resistances (Resistances (..))
import qualified Character.Spell       as Spell
import           Data.Text             as T
import           GHC.Generics          (Generic)

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
    , meleeStats  :: Phys.Stats
    , rangedStats :: Phys.Stats
    , spellStats  :: Spell.Stats
    , guild       :: Maybe T.Text
    }
  deriving (Show, Generic)

data Race
  = Human
  | Nightelf
  | Dwarf
  | Gnome
  | Orc
  | Tauren
  | Troll
  | Undead
  deriving (Show, Generic)

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
  deriving (Show, Generic)
