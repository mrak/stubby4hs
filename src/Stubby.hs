{-# LANGUAGE OverloadedStrings #-}
module Stubby (stubby) where
import CLI.Arguments
import Portal.Admin
import Portal.Stubs
import Control.Concurrent (forkIO)
import System.Console.ANSI
import Control.Monad (unless)
import Options.Applicative (execParser)

stubby :: IO ()
stubby = do
    args <- execParser arguments
    quitMessage args
    _ <- forkIO $ adminserver args
    stubserver args

quitMessage :: Arguments -> IO ()
quitMessage (Arguments _ _ q _ _) = unless q $ do
    setSGR [SetColor Foreground Dull Blue]
    putStrLn ""
    putStrLn "Quit: ctrl-c"
    putStrLn ""
    setSGR [Reset]
