{-# LANGUAGE DeriveGeneric #-}


module Spells.Spell where

import           Data.Aeson        (FromJSON (..),
                                    Options (constructorTagModifier),
                                    ToJSON (..), defaultOptions,
                                    genericParseJSON, genericToJSON)
import           Data.Aeson.Casing (camelCase)
import           GHC.Generics      (Generic)

data School
  = Arcane
  | Fire
  | Frost
  | Holy
  | Nature
  | Shadow
  deriving (Eq, Ord, Show, Generic)

instance FromJSON School where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelCase }

instance ToJSON School where
  toJSON = genericToJSON defaultOptions { constructorTagModifier = camelCase }



data SType = Direct | Duration | Buff
  deriving (Eq, Ord, Show, Generic)

data SpellClass = Harmful SType | Helpful SType
  deriving (Eq, Ord, Show, Generic)

direct :: SpellClass
direct = Harmful Direct
dot :: SpellClass
dot = Harmful Duration
debuff :: SpellClass
debuff = Harmful Buff
heal :: SpellClass
heal = Helpful Direct
hot :: SpellClass
hot = Helpful Duration
buff :: SpellClass
buff = Helpful Buff

data Spell a =
  Spell
    { school          :: School
    , sClass          :: SpellClass
    , manaCost        :: Float
    , cooldown        :: Float
    , hitBonus        :: Float
    , dmg             :: Float
    , healing         :: Float
    , duration        :: Float
    , coeff           :: Float
    , critBonus       :: Float
    , critCoeff       :: Float
    , castTime        :: Float
    , critFlatBonuses :: [Float] -- added on to the end result. Used to simulate buffs like imp shadowbolt
    , modifiers       :: [Modifier a] -- for in place adjustments at calculation time
    -- i.e. taking gear into account for calc'ing improved shadowbolt's effect
    }
  deriving (Show, Generic)

-- newtype wrapper just for the Show instances. It'd be nice to have another way to do this :(
newtype Modifier a = Modifier { unMod :: Spell a -> a -> a -> Spell a}

instance Show (Modifier a) where
  show = const "function"

mkModifiers :: [Spell a -> a -> a -> Spell a] -> [Modifier a]
mkModifiers = map Modifier

empty :: Spell a
empty =
  Spell
    { school = Arcane -- need a filler :/
    , sClass = direct -- filler again :/
    , manaCost = 0
    , cooldown = 0
    , hitBonus = 0
    , dmg = 0
    , healing = 0
    , duration = 0
    , coeff = 0
    , critBonus = 0
    , critCoeff = 1
    , castTime = 1.5 -- gcd
    , critFlatBonuses = []
    , modifiers = []
    }

beneficial :: Spell a -> Bool
beneficial Spell{sClass=c} =
  case c of
    Helpful _ -> True
    Harmful _ -> False

harmful :: Spell a -> Bool
harmful = not . beneficial

isDirect :: SpellClass -> Bool
isDirect spellClass =
  case spellClass of
    Harmful Direct -> True
    Helpful Direct -> True
    _              -> False
