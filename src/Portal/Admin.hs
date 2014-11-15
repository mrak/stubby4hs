{-# LANGUAGE OverloadedStrings #-}
module Portal.Admin (adminserver) where
import Network.Wai
import Network.HTTP.Types (status200)

adminserver :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
adminserver _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World! -- admin"
