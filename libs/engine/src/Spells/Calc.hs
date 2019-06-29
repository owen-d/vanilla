module Spells.Calc where

import           Character.Classes.Spec (Spec (..))
import           Character.Sheet        (Character (..), boss)
import           Data.Equivalence.Class (HasEP (combine))
import           Table.SpellResult      (dps)

-- | estimates dps
calc :: Spec a -> Character -> Float
calc spec char = (buffScale spec) $ dps (mkSpells spec char) char boss

derivatives :: Spec a -> Character -> [(a, Float)]
derivatives spec@ Spec{inputs=vars} char =
  map diff vars
  where
    diff point = (point, y'-y)
      where
        y = calc spec char
        y' = calc spec $ combine point char
