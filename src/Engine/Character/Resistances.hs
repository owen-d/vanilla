{-# LANGUAGE DeriveGeneric #-}

module Engine.Character.Resistances where

import           Engine.Spells.Spell (School (..))
import           GHC.Generics        (Generic)

data Resistances =
  Resistances
    { arcane :: Float
    , fire   :: Float
    , frost  :: Float
    , holy   :: Float
    , nature :: Float
    , shadow :: Float
    }
  deriving (Show, Generic)


instance Semigroup Resistances where
  a <> b =
    Resistances
      { arcane = arcane a + arcane b
      , fire = fire a + fire b
      , frost = frost a + frost b
      , nature = nature a + nature b
      , shadow = shadow a + shadow b
      , holy = holy a + holy b
      }

instance Monoid Resistances where
  mempty = Resistances 0 0 0 0 0 0

resistance :: School -> Resistances -> Float
resistance s =
  case s of
    Arcane -> arcane
    Fire   -> fire
    Frost  -> frost
    Holy   -> holy
    Nature -> nature
    Shadow -> shadow
