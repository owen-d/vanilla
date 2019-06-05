module Character.Sheet where

import           Character.Resistances (Resistances (..))

data Character =
  Character
    { stamina     :: Int
    , strength    :: Int
    , agility     :: Int
    , spirit      :: Int
    , resistances :: Resistances
    }
