{-# LANGUAGE OverloadedStrings #-}
module LibMain where

import           ApiType
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors,
                                              simpleHeaders)
import           Servant                     (Application, serve)

main :: IO ()
main = run 9000 $ cors (const $ Just corsPolicy) $ app

app :: Application
app = serve api server


corsPolicy :: CorsResourcePolicy
corsPolicy =
  CorsResourcePolicy
    { corsOrigins = Just (origins, False)
    , corsMethods = methods
    , corsRequestHeaders = simpleHeaders
    , corsExposedHeaders = Nothing
    , corsMaxAge = Just 60 -- one minute
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }
  where
    origins = ["http://localhost:3000"]
    methods = ["GET", "HEAD", "POST", "PUT", "PATCH", "OPTIONS", "DELETE"]
