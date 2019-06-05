module Character.Resistances where

data Resistances =
  Resistances
    { fire   :: Int
    , frost  :: Int
    , nature :: Int
    , shadow :: Int
    , holy   :: Int
    }


instance Semigroup Resistances where
  a <> b =
    Resistances
      { fire = fire a + fire b
      , frost = frost a + frost b
      , nature = nature a + nature b
      , shadow = shadow a + shadow b
      , holy = holy a + holy b
      }

instance Monoid Resistances where
  mempty = Resistances 0 0 0 0 0
