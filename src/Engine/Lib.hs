{-# LANGUAGE FlexibleContexts #-}

module Engine.Lib where

import           Engine.Character                   (CClass (..),
                                                     Character (..), Race (..))
import qualified Engine.Character.Classes.FireMage  as FireMage
import qualified Engine.Character.Classes.FrostMage as FrostMage
import qualified Engine.Character.Classes.Warlock   as Wlock
import qualified Engine.Character.Spell             as CSp
import           Engine.Dist                        (Distable (..))
import           Engine.PDeriv                      (Input (..), partials)
import           Engine.Spells.Spell                (School (..), Spell)
import           Engine.Table.SpellResult           (dps, spellDist)

main :: IO ()
main = sequence_ outputs

output ::
     (Show b, Fractional b, Distable c (Spell Character))
  => String
  -> [Input]
  -> Character
  -> c
  -> (Float -> b)
  -> IO ()
output label vars char sDist raidbuffs = do
  putStrLn $  "\ndps (" ++ label ++ "): " ++ (show (f char))
  mapM_ print $ partials f vars char
  where
    -- raidbuffs calcing at the end should be fine due to distributive property
    -- of multiplication over addition. Also b/c only one spell school is used
    f c = raidbuffs $ dps (toDist sDist) c boss

outputs :: [IO ()]
outputs =
  [ output
      "warlock"
      (base ++ [School Shadow])
      hero
      (Wlock.spellDist shadowDmg)
      Wlock.raidbuffs
  , output
      "frost mage"
      (base ++ [School Frost])
      hero
      (spellDist FrostMage.spellPrios)
      FrostMage.raidbuffs
  , output
      "fire mage"
      (base ++ [School Fire])
      hero
      (spellDist FireMage.spellPrios)
      FireMage.raidbuffs
  ]
  where
    base = [SpellHit, SpellCrit]
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
          { CSp.shadow = 400
          , CSp.frost = 400
          , CSp.fire = 400
          , CSp.crit = 0.14
          , CSp.hit = 0.06
          }
    , guild = Nothing
    }

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
