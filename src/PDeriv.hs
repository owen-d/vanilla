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
    updated =
      case input of
        School Arcane -> original {arcane = arcane original + 1}
        School Fire   -> original {fire = fire original + 1}
        School Frost  -> original {frost = frost original + 1}
        School Holy   -> original {holy = holy original + 1}
        School Nature -> original {nature = nature original + 1}
        School Shadow -> original {shadow = shadow original + 1}
        SpellHit      -> original {hit = hit original + 0.01}
        SpellCrit     -> original {crit = crit original + 0.01}

partials :: (Show b, Fractional b) => (Character -> b) -> Character -> IO ()
partials fn character =
  mapM_ (calc character) inputs
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
    calc c input = do
      let c' = addInput input c
      let y = fn c
      let y' = fn c'
      print (input, y' - y)
