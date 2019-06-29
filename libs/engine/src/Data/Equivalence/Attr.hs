{-# LANGUAGE DeriveGeneric #-}

module Data.Equivalence.Attr where

import           Character.Sheet        (Character (..))
import           Character.Spell        (Stats (..))
import           Data.Aeson             (FromJSON (..),
                                         Options (constructorTagModifier),
                                         ToJSON (..), defaultOptions,
                                         genericParseJSON, genericToJSON)
import           Data.Aeson.Casing      (camelCase)
import           Data.Equivalence.Class (HasEP (..))
import           GHC.Generics           (Generic)
import           Spells.Spell           (School (..))

data Attr
  = School School
  | SpellHit
  | SpellCrit
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Attr where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelCase }

instance ToJSON Attr where
  toJSON = genericToJSON defaultOptions { constructorTagModifier = camelCase }

instance HasEP Attr where
  combine attr character@Character {spellStats = original} =
    character {spellStats = updated}
    where
      updated = case attr of
          School Arcane -> original {arcane = arcane original + 1}
          School Fire   -> original {fire = fire original + 1}
          School Frost  -> original {frost = frost original + 1}
          School Holy   -> original {holy = holy original + 1}
          School Nature -> original {nature = nature original + 1}
          School Shadow -> original {shadow = shadow original + 1}
          SpellHit      -> original {hit = hit original + 0.01}
          SpellCrit     -> original {crit = crit original + 0.01}


attrs :: [Attr]
attrs =
  [ School Arcane
  , School Fire
  , School Frost
  , School Holy
  , School Nature
  , School Shadow
  , SpellHit
  , SpellCrit
  ]
