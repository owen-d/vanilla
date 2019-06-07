{-# LANGUAGE OverloadedStrings #-}

module Character.Phys where

data Stats =
  Stats
    { attackPower :: Float
    , hit         :: Float
    , crit        :: Float
    }

instance Semigroup Stats where
  a <> b =
    Stats
      { attackPower = attackPower a + attackPower b
      , hit = hit a + hit b
      , crit = crit a + crit b
      }

instance Monoid Stats where
  mempty = Stats {attackPower = 0, hit = 0, crit = 0}
