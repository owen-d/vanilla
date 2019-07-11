{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Config where
import           Data.Aeson          (FromJSON (..), Options (..), ToJSON (..),
                                      camelTo2, defaultOptions,
                                      eitherDecodeFileStrict, genericParseJSON,
                                      genericToJSON)
import qualified Data.Text           as T
import           GHC.Generics        (Generic)
import           Options.Applicative (Parser, execParser, fullDesc, helper,
                                      info, long, progDesc, short, strOption,
                                      (<**>))

data Args =
  Args
    { configFile :: String
    }

args :: Parser Args
args = Args <$> strOption (long "config-file" <> short 'c')

cli :: IO Args
cli = execParser opts
  where
    opts = info (args <**> helper) (fullDesc <> progDesc "run api")

config :: IO Config
config = cli >>= (fromFile . configFile)

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
