module PDeriv where

import           Character.Sheet (Character (..))
import           Character.Spell (Stats (..))
import           Spells.Spell    (School (..))

data Input
  = School School
  | SpellHit
  | SpellCrit
  deriving (Eq, Ord, Show)

addInput :: Input -> Character -> Character
addInput input character@Character {spellStats = original} =
  character {spellStats = updated}
  where
    -- https://vanilla-wow.fandom.com/wiki/Item_level
    sdIncrement = 14/0.86 -- 1 crit or hit = 14 pts, so 14/all schools rating
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

partials :: (Fractional b) => (Character -> b) -> Character -> [(Input, b)]
partials fn character =
  map (calc character) inputs
  where
    inputs =
      [ School Arcane
      , School Fire
      , School Frost
      , School Holy
      , School Nature
      , School Shadow
      , SpellHit
      , SpellCrit
      ]
    calc c input = (input, y'-y)
      where
        c' = addInput input c
        y = fn c
        y' = fn c'
