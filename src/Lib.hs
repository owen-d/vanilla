{-# LANGUAGE FlexibleContexts #-}

module Lib where

import           Character                   (CClass (..), Character (..),
                                              Race (..))
import qualified Character.Classes.FireMage  as FireMage
import qualified Character.Classes.FrostMage as FrostMage
import qualified Character.Classes.Warlock   as Wlock
import qualified Character.Spell             as CSp
import           Dist                        (Distable (..))
import           PDeriv                      (partials)
import           Spells.Spell                (Spell)
import           Table.SpellResult           (dps, spellDist)

main :: IO ()
main = foldl (>>) (return ()) outputs

output ::
     (Show b, Fractional b, Distable c (Spell Character))
  => String
  -> Character
  -> c
  -> (Float -> b)
  -> IO ()
output label char sDist raidbuffs = do
  putStrLn $  "\ndps (" ++ label ++ "): " ++ (show (f char))
  partials f char
  where
    -- raidbuffs calcing at the end should be fine due to distributive property
    -- of multiplication over addition. Also b/c only one spell school is used
    f c = raidbuffs $ dps (toDist sDist) c boss

outputs :: [IO ()]
outputs =
  [ output "warlock" hero (Wlock.spellDist shadowDmg) Wlock.raidbuffs
  , output "frost mage" hero (spellDist FrostMage.spellPrios) FrostMage.raidbuffs
  , output "fire mage" hero (spellDist FireMage.spellPrios) FireMage.raidbuffs
  ]
  where
    shadowDmg = CSp.shadow . spellStats $ hero


hero :: Character
hero =
  Character
    { level = 60
    , cClass = Warlock
    , race = Gnome
    , stamina = 100
    , strength = 100
    , agility = 100
    , spirit = 100
    , resistances = mempty
    , defenses = mempty
    , meleeStats = mempty
    , rangedStats = mempty
    , spellStats =
        mempty
          { CSp.shadow = 200
          , CSp.frost = 200
          , CSp.fire = 200
          , CSp.crit = 0.12
          , CSp.hit = 0.06
          }
    , guild = Nothing
    }

-- sameLevelEnemy =
--   Character
--     { level = 60
--     , cClass = Warrior
--     , race = Human
--     , stamina = 100
--     , strength = 100
--     , agility = 100
--     , spirit = 100
--     , resistances = mempty
--     , defenses = mempty
--     , meleeStats = mempty
--     , rangedStats = mempty
--     , spellStats = mempty
--     , guild = Nothing
--     }

boss :: Character
boss =
  Character
    { level = 63
    , cClass = Warrior
    , race = Human
    , stamina = 100
    , strength = 100
    , agility = 100
    , spirit = 100
    , resistances = mempty
    , defenses = mempty
    , meleeStats = mempty
    , rangedStats = mempty
    , spellStats = mempty
    , guild = Nothing
    }

-- fix :: (a -> a) -> a
-- fix f = let {x = f x} in x

-- (a -> a)
-- subsituting a = (b -> c)
-- ((b -> c) -> (b -> c)) -> (b -> c)
-- simplify: ((b -> c) -> b -> c) -> b -> c

-- z y = fix f y
--   where
--     f x =
--       if x <= 0 then x else f (x-1)

-- z x =
--   fix
--     (\f b ->
--        if b <= 0
--          then b
--          else f (b - 1))
--   x
