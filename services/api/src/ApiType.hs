{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ApiType where


import           Data.Proxy         (Proxy (..))
import qualified Routes.Derivatives as Derivatives
import           Servant            (Server)

type Api = Derivatives.Route

api :: Proxy Api
api = Proxy

server :: Server Api
server = Derivatives.handle
