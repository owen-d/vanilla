module Lib where

import           Character                 (CClass (..), Character (..),
                                            Race (..))
import qualified Character.Classes.Warlock as Wlock
import qualified Character.Spell           as Csp
import           PDeriv                    (partials)
import           Table.SpellResult         (dps)

someFunc :: IO ()
someFunc = foldl (>>) (return ()) [output]

output :: IO ()
output = do
  putStrLn $  "dps: " ++ (show (f hero))
  partials f hero
  where
    -- raidbuffs calcing at the end should be fine due to distributive property
    -- of multiplication over addition. Also b/c only one spell school is used
    f c = Wlock.raidbuffs $ dps [Wlock.curseOfDoom, Wlock.corruption, Wlock.shadowBolt] c boss

hero :: Character
hero = Character
 {level=60
 , cClass = Warlock
 , race = Gnome
 , stamina = 100
 , strength = 100
 , agility = 100
 , spirit = 100
 , resistances=mempty
 , defenses=mempty
 , meleeStats=mempty
 , rangedStats=mempty
 , spellStats=mempty{Csp.shadow=120, Csp.crit=0.12, Csp.hit=0.04}
 , guild=Nothing}

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

-- res :: Dist SpellResult
-- res = cast Wlock.shadowBolt hero boss

-- res' :: Dist SpellResult
-- res' = cast Wlock.shadowBolt{Spell.modifiers=[], Spell.critFlatBonuses=[415.03458]} hero boss
-- -- equivalent, yay!

-- d :: [Float]
-- d = map (\(_,p) -> p) $ unDist $ spellDist [Wlock.curseOfDoom, Wlock.corruption, Wlock.shadowBolt]
