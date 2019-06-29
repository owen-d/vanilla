{-# LANGUAGE DeriveGeneric #-}

module Data.Equivalence.ItemBudget where

import           Character.Sheet        (Character (..))
import           Character.Spell        (Stats (..))
import           Data.Aeson             (FromJSON (..),
                                         Options (constructorTagModifier),
                                         ToJSON (..), defaultOptions,
                                         genericParseJSON, genericToJSON)
import           Data.Aeson.Casing      (camelCase)
import           Data.Equivalence.Attr  (Attr (..), attrs)
import           Data.Equivalence.Class (HasEP (..))
import           GHC.Generics           (Generic)
import           Spells.Spell           (School (..))

newtype EqPoint = EqPoint {getAttr :: Attr}
  deriving (Show, Generic)

-- | combine will add a stat to a character, evened out for item budgets
-- so that adding any stat will consume the same budget cost
instance HasEP EqPoint where
  combine input character@Character {spellStats = original} =
    character {spellStats = updated}
      -- https://vanilla-wow.fandom.com/wiki/Item_level
    where
      sdIncrement = 14 / 0.86 -- 1 crit or hit = 14 pts, so 14/all schools rating
      updated =
        case getAttr input of
          School Arcane -> original {arcane = arcane original + sdIncrement}
          School Fire   -> original {fire = fire original + sdIncrement}
          School Frost  -> original {frost = frost original + sdIncrement}
          School Holy   -> original {holy = holy original + sdIncrement}
          School Nature -> original {nature = nature original + sdIncrement}
          School Shadow -> original {shadow = shadow original + sdIncrement}
          SpellHit      -> original {hit = hit original + 0.01}
          SpellCrit     -> original {crit = crit original + 0.01}


instance FromJSON EqPoint where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelCase }

instance ToJSON EqPoint where
  toJSON = genericToJSON defaultOptions { constructorTagModifier = camelCase }


eqPoints :: [EqPoint]
eqPoints = map EqPoint attrs
