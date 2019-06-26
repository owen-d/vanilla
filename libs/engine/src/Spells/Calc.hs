module Spells.Calc where

import           Character.Classes.Spec (Spec (..))
import           Character.Sheet        (Character (..), boss)
import           EqPoints               (EqPoint)
import qualified EqPoints               as EqPt
import           Table.SpellResult      (dps)

-- | estimates dps
calc :: Spec -> Character -> Float
calc spec char = (buffScale spec) $ dps (mkSpells spec char) char boss

derivatives :: Spec -> Character -> [(EqPoint, Float)]
derivatives spec@ Spec{attrs=vars} char =
  map diff vars
  where
    diff point = (point, y'-y)
      where
        y = calc spec char
        y' = calc spec $ EqPt.add point char
