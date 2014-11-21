{-# LANGUAGE OverloadedStrings #-}
module Stubby.Net.Admin (adminserver) where
import Prelude hiding (concat)
import Stubby.CLI.Arguments (Arguments(..))
import Stubby.CLI.Logging (status)
import Stubby.Net.LoggerMiddleware (logger)
import Network.Wai
import Network.Wai.Handler.Warp (runSettings,setHost,setPort,defaultSettings)
import Network.HTTP.Types (status200)
import Data.ByteString.Char8
import Data.String (fromString)

adminserver' :: Application
adminserver' _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World! -- admin"

adminserver :: Arguments -> IO ()
adminserver args = do
    let port = admin args
        host = location args
        settings = setHost (fromString host) $ setPort port defaultSettings
        msg = concat [ "Admin"
                        , " portal running at http://"
                        , pack host
                        , ":"
                        , pack $ show port
                        ]
    status msg
    runSettings settings $ logger args adminserver'
