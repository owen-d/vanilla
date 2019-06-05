{-# LANGUAGE OverloadedStrings #-}

module Character.Defenses where

data Defenses =
  Defenses
    { armor   :: Float
    , defense :: Float
    , dodge   :: Float
    , parry   :: Float
    , block   :: Float
    }
