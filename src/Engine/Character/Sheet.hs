{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Engine.Character.Sheet where

import           Data.Text                    as T
import           Engine.Character.Defenses    (Defenses (..))
import qualified Engine.Character.Phys        as Phys
import           Engine.Character.Resistances (Resistances (..))
import qualified Engine.Character.Spell       as Spell
import           GHC.Generics                 (Generic)

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

empty60 :: Character
empty60 =
  Character
    { level = 60
    , cClass = Warrior
    , race = Human
    , stamina = 0
    , strength = 0
    , agility = 0
    , spirit = 0
    , resistances = mempty
    , defenses = mempty
    , meleeStats = mempty
    , rangedStats = mempty
    , spellStats = mempty
    , guild = Nothing
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
