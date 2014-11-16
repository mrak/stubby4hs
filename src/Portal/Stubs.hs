{-# LANGUAGE OverloadedStrings #-}
module Portal.Stubs (stubserver) where
import CLI.Arguments
import Portal.LoggerMiddleware (logger)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)

stubserver' :: Application
stubserver' _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World! -- stubs"

stubserver :: Arguments -> IO ()
stubserver args@(Arguments _ s _ _ _) = do
    run s $ logger args "stubs" stubserver'
