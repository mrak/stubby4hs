{-# LANGUAGE OverloadedStrings #-}
module Stubby.Net.Admin (adminserver) where
import Prelude hiding (concat)
import Stubby.CLI.Settings (Settings, getAdmin, getLocation)
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

adminserver :: Settings -> IO ()
adminserver args = do
    let port = getAdmin args
        host = getLocation args
        settings = setHost (fromString host) $ setPort port defaultSettings
        msg = concat [ "Admin"
                        , " portal running at http://"
                        , pack host
                        , ":"
                        , pack $ show port
                        ]
    status msg
    runSettings settings $ logger args adminserver'
