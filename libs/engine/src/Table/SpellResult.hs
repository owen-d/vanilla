{-# LANGUAGE DeriveGeneric #-}

module Table.SpellResult where


import           Character             (Character (..))
import           Character.Resistances (resistance)
import           Character.Spell       (spellPower)
import qualified Character.Spell       as CSp
import           Dist                  (Dist (..), coalesceWith, softmax)
import           GHC.Generics          (Generic)
import           Spells.Spell          (Modifier (..), SType (..), Spell (Spell, castTime, cooldown, duration),
                                        SpellClass (..), beneficial)
import qualified Spells.Spell          as Sp
import           Util                  (notImplemented)


data SpellResolve = Miss | Hit | Crit
  deriving (Show, Eq, Ord, Generic)

-- currently we only model damage, although some types exist to specify healing/buffs
data SpellResult =
  SpellResult
    { dmg      :: Float
    , resolved :: SpellResolve
    }
  deriving (Show, Eq, Generic)

empty :: SpellResult
empty = SpellResult {dmg = 0, resolved = Miss}

isMiss :: SpellResult -> Bool
isMiss = (== Miss) . resolved

isHit :: SpellResult -> Bool
isHit = (== Hit) . resolved

isCrit :: SpellResult -> Bool
isCrit = (== Crit) . resolved

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
  -- reintroduce crit % as hits for uncrittable spells
  Harmful _      -> mitigate [miss, hits, fmap (const pCrit) hits]
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

-- expectedDmg returns the avg expected result from a distribution of [SpellResult]
-- this is mainly used for distributions based on multiple rounds
expectedDmg :: Dist [SpellResult] -> Float
expectedDmg dist =
  avg . unDist . (fmap f) $ dist
  where
    f = foldr (\x acc-> dmg x + acc) 0
    avg = foldr (\(x,p) acc -> x * p + acc) 0

-- given a list of spells in priority w/ cooldowns, yields an ideal spell distribution
spellDist :: [Spell a] -> Dist (Spell a)
spellDist = flip spellDistWithReserved 0

-- allow part of the casting-time space to be reserved (i.e. for lifetap)
spellDistWithReserved :: [Spell a] -> Float -> Dist (Spell a)
spellDistWithReserved [] _ = Dist []
spellDistWithReserved xs reserved =
  Dist $ populate maxInterval (maxInterval-reserved) xs
  where
    maxCdOrDuration s = max (cooldown s) (duration s) -- limit casts by cooldown or duration, i.e. dont cast swp until it ends
    intervals = map maxCdOrDuration xs -- how often a spell can be cast
    maxInterval = max 1.5 $ foldr max 0 intervals  -- avoid dividing by zero, so set maxCd to 1.5 (GCD)
    populate _ _ [] = []
    populate maxD timeLeft (y:ys) = (y, nTimes) : populate maxD timeLeft' ys
      where
        maxCasts = maxInterval / (maxCdOrDuration y)
        nTimes = max 0 $ min maxCasts $ timeLeft / (castTime y)
        timeLeft' = timeLeft - (nTimes * castTime y)


-- dps gets the dps for a spell prio list / caster / target combo
dps :: Dist (Spell Character) -> Character -> Character -> Float
dps sDist caster target =
  coalesceWith reducer 0 dist
  where
    dist = do
      s <- softmax sDist
      (byCastTime $ Sp.castTime s) <$> cast s caster target
    byCastTime cTime result@ SpellResult{dmg=dmgDone} = result{dmg=dmgDone/cTime}  -- adjust damage by the cast time of the spell
    reducer p SpellResult{dmg=dmgDone} acc = acc + p * dmgDone
