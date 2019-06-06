module Table.SpellResult where

import           Character             (Character (..))
import           Character.Resistances (resistance)
import           Character.Spell       (spellPower)
import qualified Character.Spell       as CSp
import           Prob                  (Prob (..))
import           Spells.Spell          (SType (..), Spell (Spell),
                                        SpellClass (..), beneficial)
import qualified Spells.Spell          as Sp
import           Util                  (notImplemented)

-- currently we only model damage, although some types exist to specify healing/buffs
data SpellResult = SpellResult Float

instance Semigroup SpellResult where
  SpellResult a <> SpellResult b =
    SpellResult $ a + b

instance Monoid SpellResult where
  mempty = SpellResult 0


hitChance :: Character -> Spell -> Character -> Float
hitChance caster spell target
  | beneficial spell = 1
  | otherwise = min 0.99 $ baseHit + spellBonus + charBonus
  where
    diff = (level target) - (level caster)
    charBonus = (CSp.hit . spellStats) caster
    spellBonus = Sp.hit spell
    baseHit
      | diff >= 3 = 0.83
      | otherwise = (96 - diff) / 100
      -- | diff == 2 = 94
      -- | diff == 1 = 95
      -- | diff == 0 = 96

-- TODO: does mob level affect crit % (outside of crit cap)?
critChance :: Character -> Spell -> Character -> Float
critChance caster spell@Spell {Sp.sClass = variant} target =
  case variant of
    Helpful Direct -> notImplemented
    Harmful Direct ->
      min (hitChance caster spell target) (charBonus + spellBonus)
    _ -> 0
  where
    charBonus = (CSp.crit . spellStats) caster
    spellBonus = Sp.crit spell



cast :: Spell -> Character -> Character -> Prob SpellResult
cast spell
  | beneficial spell = friendlyCast spell
  | otherwise = harmfulCast spell


friendlyCast :: Spell -> Character -> Character -> Prob SpellResult
friendlyCast Spell{Sp.sClass=variant} _ _ = case variant of
  Harmful _ -> fail "requires a helpful spell"
  Helpful _ -> notImplemented

harmfulCast :: Spell -> Character -> Character -> Prob SpellResult
harmfulCast spell@ Spell{Sp.sClass=variant, Sp.school=school'} caster enemy@ Character{resistances=resists} = case variant of
  Helpful _      -> fail "requires a harmful spell"
  Harmful Direct -> mitigate [miss, hits, crits]
  Harmful _      -> mitigate [miss, hits]
  where
    pHit = hitChance caster spell enemy
    pCrit = critChance caster spell enemy
    miss = (0, 1 - pHit)
    hits = (hitDmg, pHit - pCrit)
    crits = (critDmg, pCrit)
    hitDmg = (Sp.dmg spell) + (Sp.coeff spell * spellPower school' (spellStats caster))
    critDmg = hitDmg * Sp.critCoeff spell
    avgRes = max 0.75 $ (resistance school' resists) / (level caster * 5) * 0.75
    mitigate xs = Prob $ flip map xs $ \(x, p) ->
      (SpellResult (x * avgRes), p)

  -- -- miss, hit, crit
  -- let unmitigated =
  --   [ (mempty, 1 - hits)
  --   , (mempty {damage = hitDmg * avgRes}, hits - crits)
  --   , (mempty {damage = critDmg * avgRes}, crits)
  --   ]
  -- in Prob $ map mitigate unmitigated
  -- where
  --   school' = school spell
  --   hits = hitChance spell
  --   crits = min hits $ crit spell -- crit capping is unlikely.
  --   hitDmg = (dmg spell) + (coeff spell * spellPower school' stats)
  --   critDmg = hitDmg * critCoeff spell
  --   avgRes = max 0.75 $ (resistance school' res) / (cLvl spell * 5) * 0.75
  --   mitigate (a, b) = (a,b) -- applies resistances to result

