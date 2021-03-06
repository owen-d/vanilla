{-# LANGUAGE OverloadedStrings #-}

import           ApiType
import           Character.Spell            (Stats)
import           Control.Lens
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as CL
import           Data.Swagger
import           Routes.Derivatives
import           Servant.Swagger
import           Spells.Spell               (School)

main :: IO ()
main = CL.writeFile "swagger.json" (encodePretty swaggerSpec)

instance ToSchema Stats

instance ToSchema School

instance ToSchema AttrIdentifier

instance ToSchema ReqFields

instance ToSchema SpecIdentifier

instance ToSchema DpsResponse

-- | Swagger spec for Todo API.
swaggerSpec :: Swagger
swaggerSpec = toSwagger api
  & info.title   .~ "Vanilla API"
  & info.version .~ "0.1"
  & info.description ?~ "This API helps calculate gear effectiveness"
