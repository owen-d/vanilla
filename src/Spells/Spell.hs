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

data Spell =
  Spell
    { school    :: School
    , sClass    :: SpellClass
    , hit       :: Float
    , dmg       :: Float
    , healing   :: Float
    , duration  :: Float
    , coeff     :: Float
    , crit      :: Float
    , critCoeff :: Float
    , castTime  :: Float
    , effects   :: [Spell]
    }
  deriving (Eq, Ord, Show)

empty :: Spell
empty =
  Spell
    { school = Arcane -- need a filler :/
    , sClass = direct -- filler again :/
    , hit = 0
    , dmg = 0
    , healing = 0
    , duration = 0
    , coeff = 0
    , crit = 0
    , critCoeff = 0
    , castTime = 0
    , effects = []
    }

beneficial :: Spell -> Bool
beneficial Spell{sClass=c} =
  case c of
    Helpful _ -> True
    Harmful _ -> False

harmful = not . beneficial
