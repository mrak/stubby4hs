{-# LANGUAGE OverloadedStrings #-}
module Stubby (stubby) where
import CLIArgs
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import Control.Concurrent (forkIO)

stubby :: CLIArgs -> IO ()
stubby (CLIArgs a s _ _ _) = do
    _ <- forkIO $ run a adminserver
    run s stubserver

stubserver :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
stubserver _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World! -- stubs"

adminserver :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
adminserver _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World! -- admin"
