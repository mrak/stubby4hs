{-# LANGUAGE OverloadedStrings #-}
module Stubby (stubby) where
import CLI.Arguments
import CLI.Logging (info)
import Portal.Admin (adminserver)
import Portal.Stubs (stubserver)
import Control.Concurrent (forkIO)
import Control.Monad (unless)
import Options.Applicative (execParser)

stubby :: IO ()
stubby = do
    args <- execParser arguments
    unless (quiet args) quitMessage
    _ <- forkIO $ adminserver args
    stubserver args

quitMessage :: IO ()
quitMessage = info "" >> info "Quit: ctrl-c" >> info ""
