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
import           Character.Classes.Spec            (Spec (..), sMap)
import qualified Character.Classes.Warlock         as Wlock
import qualified Character.Spell                   as CSp
import           Data.Equivalence.Attr             (Attr)
import           Data.Equivalence.Class            (HasEP (..))
import           Data.Equivalence.ItemBudget       (EqPoint (EqPoint))
import           Spells.Calc                       (calc, derivatives)

main :: IO ()
main = byAttr

byAttr :: IO ()
byAttr = sequence_ $ outputs id

byEq :: IO ()
byEq = sequence_ $ outputs EqPoint

output :: Show a => String -> Spec a -> Character -> IO ()
output label spec char = do
  putStrLn $ "\ndps (" ++ label ++ "): " ++ (show $ calc spec char)
  mapM_ (putStrLn . show) (derivatives spec char)

outputs :: (Show b, HasEP b) => (Attr -> b) -> [IO ()]
outputs f = flip map specPairs $
  \(str, spec) -> output str (sMap f spec) hero

specPairs :: [(String, Spec Attr)]
specPairs =
  [  ("warlock", Wlock.spec)
  ,  ("frost mage", FrostMage.spec)
  ,  ("arcane mage", ArcaneMage.spec)
  ,  ("fire mage", FireMage.spec)
  ,  ("moonkin", Moonkin.spec)
  ,  ("ele sham", EleSham.spec)
  ,  ("shadow priest", SPriest.spec)
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
