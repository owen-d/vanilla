module Spells.Spell where

data School
  = Arcane
  | Fire
  | Frost
  | Holy
  | Nature
  | Shadow
  deriving (Eq, Ord, Show)

data SType = Direct | Duration | Buff
  deriving (Eq, Ord, Show)

data SpellClass = Harmful SType | Helpful SType
  deriving (Eq, Ord, Show)

direct = Harmful Direct
dot = Harmful Duration
debuff = Harmful Buff
heal = Helpful Direct
hot = Helpful Duration
buff = Helpful Buff

data Spell a =
  Spell
    { school          :: School
    , sClass          :: SpellClass
    , hitBonus        :: Float
    , dmg             :: Float
    , healing         :: Float
    , duration        :: Float
    , coeff           :: Float
    , critBonus       :: Float
    , critCoeff       :: Float
    , castTime        :: Float
    , critFlatBonuses :: [Float] -- added on to the end result. Used to simulate buffs like imp shadowbolt
    , modifiers       :: [(Spell a -> a -> a -> Spell a)] -- for in place adjustments at calculation time
    -- i.e. taking gear into account for calc'ing improved shadowbolt's effect
    }

empty :: Spell a
empty =
  Spell
    { school = Arcane -- need a filler :/
    , sClass = direct -- filler again :/
    , hitBonus = 0
    , dmg = 0
    , healing = 0
    , duration = 0
    , coeff = 0
    , critBonus = 0
    , critCoeff = 1
    , castTime = 0
    , critFlatBonuses = []
    , modifiers = []
    }

beneficial :: Spell a -> Bool
beneficial Spell{sClass=c} =
  case c of
    Helpful _ -> True
    Harmful _ -> False

harmful = not . beneficial
