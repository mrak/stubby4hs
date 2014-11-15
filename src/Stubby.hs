{-# LANGUAGE OverloadedStrings #-}
module Stubby (stubby) where
import CLI.Arguments
import Portal.Admin
import Portal.Stubs
import Portal.LoggerMiddleware
import Network.Wai.Handler.Warp (run)
import Control.Concurrent (forkIO)

stubby :: Arguments -> IO ()
stubby (Arguments a s _ _ _) = do
    _ <- forkIO $ run a $ logger "admin" adminserver
    run s $ logger "stubs" stubserver
