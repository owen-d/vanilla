{-# LANGUAGE OverloadedStrings #-}

module Character.Spell where

import           Spells.Spell (School (..))

-- TODO: hit/crit should be affectable at the school and spell level, not just at the character-sheet
data Stats =
  Stats
    { arcane  :: Float
    , fire    :: Float
    , frost   :: Float
    , holy    :: Float
    , nature  :: Float
    , shadow  :: Float
    , healing :: Float
    , hit     :: Float
    , pen     :: Float
    , crit    :: Float
    }

base :: Stats
base =
  Stats
    { arcane = 0
    , fire = 0
    , frost = 0
    , holy = 0
    , nature = 0
    , shadow = 0
    , healing = 0
    , hit = 0
    , pen = 0
    , crit = 0
    }

spellPower :: School -> Stats -> Float
spellPower s =
  case s of
    Arcane -> arcane
    Fire   -> fire
    Frost  -> frost
    Holy   -> holy
    Nature -> nature
    Shadow -> shadow
