{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes.Derivatives where

import           Character                         (Character (spellStats),
                                                    empty60)
import qualified Character.Classes.ArcaneMage      as ArcaneMage
import qualified Character.Classes.BalanceDruid    as Boomkin
import qualified Character.Classes.ElementalShaman as EleSham
import qualified Character.Classes.FireMage        as FireMage
import qualified Character.Classes.FrostMage       as FrostMage
import qualified Character.Classes.ShadowPriest    as SPriest
import           Character.Classes.Spec            (Spec)
import qualified Character.Classes.Warlock         as Warlock
import           Character.Spell                   (Stats (..))
import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.Equivalence.Attr             (Attr)
import qualified Data.Equivalence.Attr             as Attr
import           Data.Tuple                        (swap)
import           GHC.Generics                      (Generic)
import           Servant                           (Server)
import           Servant.API                       ((:>), JSON, Post, ReqBody)
import           Spells.Calc                       (calc, derivatives)
import qualified Spells.Spell                      as Spell

type Routes =
  "dps" :> ReqBody '[JSON] ReqFields :> Post '[JSON] DpsResponse

handlers :: Server Routes
handlers = handleDPS

handleDPS :: Monad m => ReqFields -> m DpsResponse
handleDPS ReqFields {stats = stats', spec = identifier} =
  return $ DpsResponse {dps=dps', partialDerivatives=pDerivs}
  where
    char = empty60 {spellStats = stats'}
    dps' = calc (toSpec identifier) char
    mkAttrId = swap . (fmap toAttrId) . swap -- swapping for (a,b)'s functor impl
    pDerivs = map mkAttrId $ derivatives (toSpec identifier) char

data ReqFields =
  ReqFields
    { stats :: Stats
    , spec  :: SpecIdentifier
    }
  deriving (Show, Generic)

instance FromJSON ReqFields

data DpsResponse =
  DpsResponse
    { dps                :: Float
    , partialDerivatives :: [(AttrIdentifier, Float)]
    }
  deriving (Show, Generic)

instance ToJSON DpsResponse

-- | swagger2 has difficulties generating schemas for mixed sum types
-- (with both unit and non-unit constructors). We circumvent this by
-- having a verbose intermediary type
data AttrIdentifier
  = SpellHit
  | SpellCrit
  | Arcane
  | Fire
  | Frost
  | Holy
  | Nature
  | Shadow
  deriving (Show, Generic)

instance FromJSON AttrIdentifier
instance ToJSON AttrIdentifier

toAttr :: AttrIdentifier -> Attr
toAttr x = case x of
  SpellHit  -> Attr.SpellHit
  SpellCrit -> Attr.SpellCrit
  Arcane    -> Attr.School Spell.Arcane
  Fire      -> Attr.School Spell.Fire
  Frost     -> Attr.School Spell.Frost
  Holy      -> Attr.School Spell.Holy
  Nature    -> Attr.School Spell.Nature
  Shadow    -> Attr.School Spell.Shadow

toAttrId :: Attr -> AttrIdentifier
toAttrId x = case x of
 Attr.SpellHit            ->   SpellHit
 Attr.SpellCrit           ->   SpellCrit
 Attr.School Spell.Arcane ->   Arcane
 Attr.School Spell.Fire   ->   Fire
 Attr.School Spell.Frost  ->   Frost
 Attr.School Spell.Holy   ->   Holy
 Attr.School Spell.Nature ->   Nature
 Attr.School Spell.Shadow ->   Shadow

data SpecIdentifier
  = FireMage
  | FrostMage
  | ArcaneMage
  | Warlock
  | BalanceDruid
  | ElementalShaman
  | ShadowPriest
  deriving (Show, Generic)

instance FromJSON SpecIdentifier where

instance ToJSON SpecIdentifier where

toSpec :: SpecIdentifier -> Spec Attr
toSpec x = case x of
  FireMage        -> FireMage.spec
  FrostMage       -> FrostMage.spec
  ArcaneMage      -> ArcaneMage.spec
  Warlock         -> Warlock.spec
  BalanceDruid    -> Boomkin.spec
  ElementalShaman -> EleSham.spec
  ShadowPriest    -> SPriest.spec

