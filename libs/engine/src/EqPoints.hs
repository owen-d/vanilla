module EqPoints where

import           Character.Sheet (Character (..))
import           Character.Spell (Stats (..))
import           Spells.Spell    (School (..))

data EqPoint
  = School School
  | SpellHit
  | SpellCrit
  deriving (Eq, Ord, Show)

eqPoints :: [EqPoint]
eqPoints =
  [ School Arcane
  , School Fire
  , School Frost
  , School Holy
  , School Nature
  , School Shadow
  , SpellHit
  , SpellCrit
  ]

-- | add will add a stat to a character, evened out for item budgets
-- | so that adding any stat will consume the same budget cost
add :: EqPoint -> Character -> Character
add input character@Character {spellStats = original} =
  character {spellStats = updated}
    -- https://vanilla-wow.fandom.com/wiki/Item_level
  where
    sdIncrement = 14 / 0.86 -- 1 crit or hit = 14 pts, so 14/all schools rating
    updated =
      case input of
        School Arcane -> original {arcane = arcane original + sdIncrement}
        School Fire   -> original {fire = fire original + sdIncrement}
        School Frost  -> original {frost = frost original + sdIncrement}
        School Holy   -> original {holy = holy original + sdIncrement}
        School Nature -> original {nature = nature original + sdIncrement}
        School Shadow -> original {shadow = shadow original + sdIncrement}
        SpellHit      -> original {hit = hit original + 0.01}
        SpellCrit     -> original {crit = crit original + 0.01}

