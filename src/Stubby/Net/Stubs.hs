{-# LANGUAGE OverloadedStrings #-}
module Stubby.Net.Stubs (stubserver) where
import Prelude hiding (concat)
import Stubby.CLI.Settings (Settings, getStubs, getLocation)
import Stubby.CLI.Logging (status)
import Stubby.Net.LoggerMiddleware (logger)
import Network.Wai
import Network.Wai.Handler.Warp (runSettings,setHost,setPort,defaultSettings)
import Network.HTTP.Types (status200)
import Data.ByteString.Char8
import Data.String (fromString)

stubserver' :: Application
stubserver' _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World! -- stubs"

stubserver :: Settings -> IO ()
stubserver args = do
    let port = getStubs args
        host = getLocation args
        settings = setHost (fromString host) $ setPort port defaultSettings
        msg = concat [ "Stubs"
                        , " portal running at http://"
                        , pack host
                        , ":"
                        , pack $ show port
                        ]
    status msg
    runSettings settings $ logger args stubserver'
