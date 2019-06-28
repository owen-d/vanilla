{-# LANGUAGE FlexibleContexts #-}

module Lib where

import           Character                         (CClass (..), Character (..),
                                                    Race (..))
import qualified Character.Classes.ArcaneMage      as ArcaneMage
import qualified Character.Classes.BalanceDruid    as Moonkin
import qualified Character.Classes.ElementalShaman as EleSham
import qualified Character.Classes.FireMage        as FireMage
import qualified Character.Classes.FrostMage       as FrostMage
import qualified Character.Classes.ShadowPriest    as SPriest
import           Character.Classes.Spec            (Spec (..))
import qualified Character.Classes.Warlock         as Wlock
import qualified Character.Spell                   as CSp
import           Spells.Calc                       (calc, derivatives)

main :: IO ()
main = sequence_ outputs

output :: String -> Spec -> Character -> IO ()
output label spec char = do
  putStrLn $ "\ndps (" ++ label ++ "): " ++ (show $ calc spec char)
  mapM_ (putStrLn . show) (derivatives spec char)

outputs :: [IO ()]
outputs =
  [ output "warlock" Wlock.spec hero
  , output "frost mage" FrostMage.spec hero
  , output "arcane mage" ArcaneMage.spec hero
  , output "fire mage" FireMage.spec hero
  , output "moonkin" Moonkin.spec hero
  , output "ele sham" EleSham.spec hero
  , output "shadow priest" SPriest.spec hero
  ]


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
          , CSp.arcane = 200
          , CSp.nature = 200
          , CSp.crit = 0.15
          , CSp.hit = 0.04
          }
    , guild = Nothing
    }
