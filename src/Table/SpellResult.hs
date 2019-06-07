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
data SpellResult =
  SpellResult
    { dmg      :: Float
    , resolved :: SpellResolve
    }

data SpellResolve = Miss | Hit | Crit
  deriving Eq

-- always prefer crit then hit then miss. Keeps track of best result
instance Semigroup SpellResolve where
  Miss <> x = x
  Hit <> Miss = Hit
  Hit <> x = x
  Crit <> _ = Crit

hitChance :: Character -> Spell Character -> Character -> Float
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
critChance :: Character -> Spell Character -> Character -> Float
critChance caster spell@Spell {Sp.sClass = variant} target =
  case variant of
    Helpful Direct -> notImplemented
    Harmful Direct ->
      min (hitChance caster spell target) (charBonus + spellBonus)
    _ -> 0
  where
    charBonus = (CSp.crit . spellStats) caster
    spellBonus = Sp.critBonus spell



cast :: Spell Character -> Character -> Character -> Prob SpellResult
cast spell
  | beneficial spell = friendlyCast spell
  | otherwise = harmfulCast spell


friendlyCast :: Spell Character -> Character -> Character -> Prob SpellResult
friendlyCast Spell{Sp.sClass=variant} _ _ = case variant of
  Harmful _ -> fail "requires a helpful spell"
  Helpful _ -> notImplemented

harmfulCast :: Spell Character -> Character -> Character -> Prob SpellResult
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
    avgRes = max 0.75 $ (resistance school' resists) / (level caster * 5) * 0.75
    mitigate xs = Prob $ flip map xs $ \(x, p) ->
      (x{dmg=dmg x * avgRes}, p)

-- modify applies all modifiers, returning the adjusted spell.
modify :: Spell Character -> Character -> Character -> Spell Character
modify spell@Spell {Sp.modifiers = mods} caster target =
  case mods of
    [] -> spell
    (f:fs) -> modify spell' caster target
      where spell' = f spell {Sp.modifiers = fs} caster target
