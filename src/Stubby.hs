{-# LANGUAGE OverloadedStrings #-}
import Stubby.CLI.Arguments
import Stubby.CLI.Logging (info)
import Stubby.Net.Admin (adminserver)
import Stubby.Net.Stubs (stubserver)
import Control.Concurrent (forkIO)
import Control.Monad (unless)
import Options.Applicative (execParser)

main :: IO ()
main = do
    args <- execParser arguments
    unless (quiet args) quitMessage
    _ <- forkIO $ adminserver args
    stubserver args

quitMessage :: IO ()
quitMessage = info "" >> info "Quit: ctrl-c" >> info ""
