{-# LANGUAGE OverloadedStrings #-}
module Portal.Stubs (stubserver) where
import Network.Wai
import Network.HTTP.Types (status200)

stubserver :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
stubserver _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World! -- stubs"
