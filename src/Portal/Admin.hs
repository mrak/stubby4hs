{-# LANGUAGE OverloadedStrings #-}
module Portal.Admin (adminserver) where
import CLI.Arguments
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Portal.LoggerMiddleware (logger)

adminserver' :: Application
adminserver' _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World! -- admin"

adminserver :: Arguments -> IO ()
adminserver args@(Arguments a _ _ _ _) =
    run a $ logger args "admin" adminserver'
