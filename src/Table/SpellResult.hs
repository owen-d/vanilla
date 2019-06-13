{-# LANGUAGE DeriveGeneric #-}

module Table.SpellResult where


import           Character             (Character (..))
import           Character.Resistances (resistance)
import           Character.Spell       (spellPower)
import qualified Character.Spell       as CSp
import           Dist                  (Dist (..), distWhere, rounds)
import           GHC.Generics          (Generic)
import           Spells.Spell          (Modifier (..), SType (..),
                                        Spell (Spell, castTime, cooldown),
                                        SpellClass (..), beneficial)
import qualified Spells.Spell          as Sp
import           Util                  (notImplemented)

-- currently we only model damage, although some types exist to specify healing/buffs
data SpellResult =
  SpellResult
    { dmg      :: Float
    , resolved :: SpellResolve
    }
  deriving (Show, Eq, Generic)

empty :: SpellResult
empty = SpellResult {dmg = 0, resolved = Miss}

data SpellResolve = Miss | Hit | Crit
  deriving (Show, Eq, Ord, Generic)

-- always prefer crit then hit then miss. Keeps track of best result
instance Semigroup SpellResolve where
  (<>) = max

hitChance :: Character -> Spell a -> Character -> Float
hitChance caster spell target
  | beneficial spell = 1
  | otherwise = min 0.99 $ baseHit + spellBonus + charBonus
  where
    diff = (level target) - (level caster)
    charBonus = (CSp.hit . spellStats) caster
    spellBonus = Sp.hitBonus spell
    baseHit
      | diff >= 3 = 0.83
      | otherwise = (96 - diff) / 100
      -- | diff == 2 = 94
      -- | diff == 1 = 95
      -- | diff == 0 = 96

-- TODO: does mob level affect crit % (outside of crit cap)?
critChance :: Character -> Spell a -> Character -> Float
critChance caster spell@Spell {Sp.sClass = variant} target =
  case variant of
    Helpful Direct -> p
    Harmful Direct -> p
    _              -> 0
  where
    p = min (hitChance caster spell target) (charBonus + spellBonus)
    charBonus = (CSp.crit . spellStats) caster
    spellBonus = Sp.critBonus spell



cast :: Spell Character -> Character -> Character -> Dist SpellResult
cast spell
  | beneficial spell = friendlyCast spell
  | otherwise = harmfulCast spell


friendlyCast :: Spell Character -> Character -> Character -> Dist SpellResult
friendlyCast Spell{Sp.sClass=variant} _ _ = case variant of
  Harmful _ -> fail "requires a helpful spell"
  Helpful _ -> notImplemented

harmfulCast :: Spell Character -> Character -> Character -> Dist SpellResult
harmfulCast s caster enemy@ Character{resistances=resists}=
  case variant of
  Helpful _      -> fail "requires a harmful spell"
  Harmful Direct -> mitigate [miss, hits, crits]
  Harmful _      -> mitigate [miss, hits]
  where
    spell@ Spell{Sp.sClass=variant, Sp.school=school', Sp.critFlatBonuses=cfbs} = modify s caster enemy
    pHit = hitChance caster spell enemy
    pCrit = critChance caster spell enemy
    miss = (SpellResult{dmg=0, resolved=Miss}, 1 - pHit)
    hits = (SpellResult{dmg=hitDmg, resolved=Hit}, pHit - pCrit)
    crits = (SpellResult{dmg=critDmg + sum cfbs , resolved=Crit}, pCrit)
    hitDmg = (Sp.dmg spell) + (Sp.coeff spell * spellPower school' (spellStats caster))
    critDmg = hitDmg * Sp.critCoeff spell
    avgRes = min 0.75 $ (resistance school' resists) / (level caster * 5) * 0.75
    resCoeff = 1 - avgRes
    mitigate xs = Dist $ flip map xs $ \(x, p) ->
      (x{dmg=dmg x * resCoeff}, p)

-- modify applies all modifiers, returning the adjusted spell.
modify :: Spell Character -> Character -> Character -> Spell Character
modify spell@Spell {Sp.modifiers = mods} caster target =
  case mods of
    [] -> spell
    (f:fs) -> modify spell' caster target
      where spell' = (unMod f) spell {Sp.modifiers = fs} caster target

-- expected returns the avg expected result from a distribution of [SpellResult]
-- this is mainly used for distributions based on multiple rounds
expectedDmg :: Dist [SpellResult] -> Float
expectedDmg dist =
  avg . unDist . (fmap f) $ dist
  where
    f = foldr (\x acc-> dmg x + acc) 0
    avg = foldr (\(x,p) acc -> x * p + acc) 0
-- expected = avg . unDist
--   where
--     avg = foldr (\(SpellResult{dmg=x}, p) acc -> acc + x * p) 0

-- given a round distribution, yield the sub-distribution that contains up to N critical strikes
maxCritN :: Dist SpellResult -> Int -> Int -> Dist [SpellResult]
maxCritN dist nRounds maxN =
  distWhere predicate populated
  where
    predicate xs = length (filter isCrit xs) <= maxN
    isCrit = (== Crit) . resolved
    populated = rounds nRounds dist

{-
need a fn which, given a list of spells in priority w/ cooldowns, determines a probability ratio of the next spell to cast

- find lowest common multiple of cooldowns
- in descending priority, max out each spell by finding the quotient: LCMDuration/cd
  - keep track of how much space is used. when space is consumed, exit
-}
spellDist :: [Spell a] -> Dist (Spell a)
spellDist [] = Dist []
spellDist xs = Dist $ pull maxCd xs
  where
    max' []     = 0
    max' (y:ys) = max y $ max' ys
    maxCd = max' $ map cooldown xs
    pull _ [] = []
    pull rest (y:ys) = (y, nTimes) : pull (left - nTimes * left) ys
      where
        maxTimes = (cooldown y) / maxCd -- max number of times spell can be cast in entire duration
        nTimes = max (maxTimes) $ (castTime y) / rest -- bound by remaining duration
        left = left - (nTimes * castTime y)
