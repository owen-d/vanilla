{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Routes.Derivatives where

import           Character                   (Character (spellStats), empty60)
import qualified Character.Classes.FireMage  as FireMage
import qualified Character.Classes.FrostMage as FrostMage
import           Character.Classes.Spec      (Spec)
import qualified Character.Classes.Warlock   as Warlock
import           Character.Spell             (Stats (..))
import           Data.Aeson                  (FromJSON, ToJSON)
import           EqPoints                    (EqPoint)
import           GHC.Generics                (Generic)
import           Servant                     (Server)
import           Servant.API                 ((:<|>) (..), (:>), JSON, Post,
                                              ReqBody)
import           Spells.Calc                 (calc, derivatives)

type Routes =
  "equivalence" :> ReqBody '[JSON] ReqFields :> Post '[JSON] [(EqPoint, Float)]
  :<|> "dps" :> ReqBody '[JSON] ReqFields :> Post '[JSON] Float

handlers :: Server Routes
handlers = handleDerivatives
  :<|> handleDPS

handleDerivatives :: Monad m => ReqFields -> m [(EqPoint, Float)]
handleDerivatives ReqFields {stats = stats', spec = identifier} =
  return $ derivatives (toSpec identifier) char
  where
    char = empty60 {spellStats = stats'}

handleDPS :: Monad m => ReqFields -> m Float
handleDPS ReqFields {stats = stats', spec = identifier} =
  return $ calc (toSpec identifier) char
  where
    char = empty60 {spellStats = stats'}

data ReqFields =
  ReqFields
    { stats :: Stats
    , spec  :: SpecIdentifier
    }
  deriving (Show, Generic)

instance FromJSON ReqFields
instance ToJSON ReqFields

data SpecIdentifier = FireMage | FrostMage | Warlock
  deriving (Show, Generic)

instance FromJSON SpecIdentifier where

instance ToJSON SpecIdentifier where

toSpec :: SpecIdentifier -> Spec
toSpec x = case x of
  FireMage  -> FireMage.fireMage
  FrostMage -> FrostMage.frostMage
  Warlock   -> Warlock.warlock

