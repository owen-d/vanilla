{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiType where


import           Data.Proxy         (Proxy (..))
import qualified Routes.Derivatives as Derivatives
import           Servant            (Server)

type Api = Derivatives.Routes

api :: Proxy Api
api = Proxy

server :: Server Api
server = Derivatives.handlers
