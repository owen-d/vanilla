{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Engine.Character.Defenses where

import           GHC.Generics (Generic)

data Defenses =
  Defenses
    { armor   :: Float
    , defense :: Float
    , dodge   :: Float
    , parry   :: Float
    , block   :: Float
    }
  deriving (Show, Generic)

instance Semigroup Defenses where
  a <> b =
    Defenses
      { armor = armor a + armor b
      , defense = defense a + defense b
      , dodge = dodge a + dodge b
      , parry = parry a + parry b
      , block = block a + block b
      }

instance Monoid Defenses where
  mempty = Defenses 0 0 0 0 0
