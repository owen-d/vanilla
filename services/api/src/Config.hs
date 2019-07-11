{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where
import           Data.Aeson
import qualified Data.Text    as T
import           GHC.Generics (Generic)

data Config =
  Config
    { corsOrigins :: Maybe [T.Text]
    , port        :: Int
    }
  deriving (Show, Generic)

jsonOpts :: Options
jsonOpts = defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance FromJSON Config where
  parseJSON = genericParseJSON  jsonOpts

instance ToJSON Config where
  toJSON = genericToJSON  jsonOpts

fromFile :: FilePath -> IO Config
fromFile file = eitherDecodeFileStrict file >>= bail

bail :: (Show a, Monad m) => Either a b -> m b
bail x = case x of
  Left e  -> fail $ show e
  Right y -> return y
