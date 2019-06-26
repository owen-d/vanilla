module Character.Classes.Spec where

import           Character    (Character)
import           Dist         (Dist)
import           EqPoints     (EqPoint (..))
import           Spells.Spell (Spell)

data Spec =
  Spec
    { attrs     :: [EqPoint]
    , mkSpells  :: Character -> Dist (Spell Character)
    , buffScale :: Float -> Float
    }
