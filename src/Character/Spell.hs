{-# LANGUAGE OverloadedStrings #-}

module Character.Spell where

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

data School
  = Arcane
  | Fire
  | Frost
  | Holy
  | Nature
  | Shadow

spellPower :: School -> Stats -> Float
spellPower s =
  case s of
    Arcane -> arcane
    Fire   -> fire
    Frost  -> frost
    Holy   -> holy
    Nature -> nature
    Shadow -> shadow
