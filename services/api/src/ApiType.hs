{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ApiType where


import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Proxy   (Proxy (..))
import qualified Data.Text    as T
import           GHC.Generics (Generic)
import           Servant      (Server)
import           Servant.API

type Api = "users" :> Get '[JSON] [User]

api :: Proxy Api
api = Proxy

data User =
  User
    { name :: T.Text
    , age  :: Int
    }
  deriving (Generic, Show)

instance FromJSON User
instance ToJSON User

users :: [User]
users = [User "john" 30, User "jane" 30]

server :: Server Api
server = return users
