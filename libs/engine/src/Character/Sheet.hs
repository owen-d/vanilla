{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Character.Sheet where

import           Character.Defenses    (Defenses (..))
import qualified Character.Phys        as Phys
import           Character.Resistances (Resistances (..))
import qualified Character.Spell       as Spell
import           Data.Text             as T
import           GHC.Generics          (Generic)

class HasLevel a where
  getLevel :: a -> Float

class HasSpellStats a where
  getSpellStats :: a -> Spell.Stats

instance HasLevel Character where
  getLevel = level

instance HasSpellStats Character where
  getSpellStats = spellStats

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

boss :: Character
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
