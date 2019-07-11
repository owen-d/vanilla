{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module LibMain where

import           ApiType
import           Config                      (Config (Config), fromFile)
import qualified Config                      as Config
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Trans.Reader  (ReaderT (..), ask)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (CorsResourcePolicy (..), Origin,
                                              cors, simpleHeaders)
import           Servant                     (serve)

main :: IO ()
main = do
  conf <- fromFile "/tmp/conf.json"
  runReaderT app conf

app :: MonadIO m => ReaderT Config m ()
app = do
  Config {Config.port = port, Config.corsOrigins = origins} <- ask
  let origins' = fmap toOrigins origins
  let policy = const $ Just $ withOrigins origins'
  liftIO . (run port) . (cors policy) . (serve api) $ server


toOrigins :: [T.Text] -> ([Origin], Bool)
toOrigins origins = (fmap TE.encodeUtf8 origins, False)

withOrigins :: Maybe ([Origin], Bool) -> CorsResourcePolicy
withOrigins origins =
  CorsResourcePolicy
    { corsOrigins = origins
    , corsMethods = methods
    , corsRequestHeaders = simpleHeaders
    , corsExposedHeaders = Nothing
    , corsMaxAge = Just 60 -- one minute
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }
  where
    methods = ["GET", "HEAD", "POST", "PUT", "PATCH", "OPTIONS", "DELETE"]

