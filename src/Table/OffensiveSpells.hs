{-# LANGUAGE OverloadedStrings #-}

module Table.OffensiveSpells where

import           Character.Resistances (Resistances, resistance)
import           Character.Spell       (School (..), Stats, spellPower)
import           Prob                  (Prob (..))

-- modifiers should be in fractional form (i.e. 4% hit -> 0.04)
data Spell =
  Spell
    { school    :: School
    , effect    :: [Effect]
    , hit       :: Float
    , dmg       :: Float
    , coeff     :: Float
    , pen       :: Float
    , crit      :: Float
    , critCoeff :: Float
    , cLvl      :: Float -- caster level
    , tLvl      :: Float -- target level
    }

-- Effects model ancillary things that aren't direct damage (debuffs)
data Effect = Effect


{- NOTES

https://wowwiki.fandom.com/wiki/Resistance?oldid=398985
basic resistance is given by Average Resistance = (Target's Resistance / (Caster's Level * 5)) * 0.75
regardless of resistance level, the resistance effect caps out at 75%

-}

data SpellResult =
  SpellResult
    { damage  :: Float
    , effects :: [Effect]
    }

instance Semigroup SpellResult where
  a <> b =
    SpellResult {damage = damage a + damage b, effects = effects a ++ effects b}

instance Monoid SpellResult where
  mempty = SpellResult 0 []

miss :: SpellResult
miss = SpellResult {damage = 0, effects = []}

hitChance :: Spell -> Float
hitChance s = max 0.99 $ baseHit + hits
  where
    diff = (tLvl s) - (cLvl s)
    hits = hit s
    baseHit
      | diff >= 3 = 0.83
      | otherwise = 96 - diff
      -- | diff == 2 = 94
      -- | diff == 1 = 95
      -- | diff == 0 = 96


-- cast models the nondeterministic results of a spell against a target
cast :: Spell -> Stats -> Resistances -> Prob SpellResult
cast spell stats res
 =
  -- miss, hit, crit
  Prob
    [ (mempty, 1 - hits)
    , (mempty {damage = hitDmg * avgRes}, crits)
    , (mempty {damage = critDmg * avgRes}, hits - crits)
    ]
  where
    school' = school spell
    hits = hitChance spell
    crits = min hits $ crit spell -- crit capping is unlikely.
    hitDmg = (dmg spell) + (coeff spell * spellPower school' stats)
    critDmg = hitDmg * critCoeff spell
    avgRes = max 0.75 $ (resistance school' res) / (cLvl spell * 5) * 0.75
