{-# LANGUAGE OverloadedStrings #-}
module CLI.Logging ( log
                   , status
                   , info
                   , ok
                   , error
                   , warn
                   , incoming
                   , stored
                   ) where
import Prelude hiding (error,log,putStrLn,putStr,concat)
import System.Console.ANSI
import Data.ByteString.Char8

log :: ByteString -> IO ()
log = colorStrLn Dull White

status :: ByteString -> IO ()
status = colorStrLn Vivid Black

info :: ByteString -> IO ()
info = colorStrLn Dull Blue

ok :: ByteString -> IO ()
ok = colorStrLn Dull Green

error :: ByteString -> IO ()
error = colorStrLn Dull Red

warn :: ByteString -> IO ()
warn = colorStrLn Dull Red

incoming :: ByteString -> IO ()
incoming = colorStrLn Dull Cyan

stored :: ByteString -> IO ()
stored = colorStrLn Dull Magenta

colorStrLn :: ColorIntensity -> Color -> ByteString -> IO ()
colorStrLn fgi fg s = do
    let color = pack $ setSGRCode [SetColor Foreground fgi fg]
        reset = pack $ setSGRCode [Reset]
    putStrLn $ concat [color,s,reset]
