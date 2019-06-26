module LibMain where

import           ApiType
import           Network.Wai.Handler.Warp (run)
import           Servant                  (Application, serve)

main :: IO ()
main = run 8080 app

app :: Application
app = serve api server
