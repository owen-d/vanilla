{-# LANGUAGE OverloadedStrings #-}

module Character.Phys where

data Stats =
  Stats
    { attackPower :: Float
    , hit         :: Float
    , crit        :: Float
    }
