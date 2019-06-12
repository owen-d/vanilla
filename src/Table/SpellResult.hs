{-# LANGUAGE DeriveGeneric #-}

module Table.SpellResult where


import           Character             (Character (..))
import           Character.Resistances (resistance)
import           Character.Spell       (spellPower)
import qualified Character.Spell       as CSp
import qualified Control.Applicative   as Applicative
import           Dist                  (Dist (..))
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
    Helpful Direct -> notImplemented
    Harmful Direct ->
      min (hitChance caster spell target) (charBonus + spellBonus)
    _ -> 0
  where
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

-- expected returns the avg expected result for a spellresult distribution
expected :: Dist SpellResult -> Float
expected = avg . unDist
  where
    avg = foldr (\(SpellResult{dmg=x}, p) acc -> acc + x * p) 0


-- maxCritOnce takes SpellResult probability distribution and returns the probability distribution
-- that includes up to one crit over the specified number of rounds
maxCrit1 :: Dist SpellResult -> Int -> Dist SpellResult
maxCrit1 _ 0 = Applicative.empty
maxCrit1 dRound n = runRound dRound (n - 1)
  where
    runRound d 0 = d
    runRound d n = do
      res@ SpellResult{dmg=dmg, resolved=t} <- d
      SpellResult{dmg=dmg', resolved=t'} <- dRound
      if Crit == t
        then return res
        else
          let d' = return SpellResult{dmg = dmg + dmg', resolved=t <> t'}
          in runRound d' (n - 1)


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
    max' (x:xs) = max x $ max' xs
    maxCd = max' $ map cooldown xs
    pull _ [] = []
    pull rest (x:xs) = (x, nTimes) : pull (left - nTimes * left) xs
      where
        maxTimes = (cooldown x) / maxCd -- max number of times spell can be cast in entire duration
        nTimes = max (maxTimes) $ (castTime x) / rest -- bound by remaining duration
        left = left - (nTimes * castTime x)
