{-# LANGUAGE OverloadedStrings #-}
module Portal.LoggerMiddleware (logger) where
import Prelude hiding (putStr, putStrLn, concat)
import System.Console.ANSI
import Network.Wai (Request(..), Middleware)
import Control.Monad.IO.Class (liftIO)
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale
import Data.ByteString.Char8

colorStr :: ColorIntensity -> Color -> ByteString -> IO ()
colorStr fgi fg s = do
    setSGR [SetColor Foreground fgi fg]
    putStr s
    setSGR []

colorStrLn :: ColorIntensity -> Color -> ByteString -> IO ()
colorStrLn fgi fg s = colorStr fgi fg s >> putStrLn ""

logger :: ByteString -> Middleware
logger name app req sendResponse = do
    t <- getZonedTime
    let method = requestMethod req
        path   = rawPathInfo req
        time   = pack $ formatTime defaultTimeLocale "%T" t
        msg    = concat [time," -> ",method," ",name,path]
    liftIO $ colorStrLn Dull Cyan msg
    app req sendResponse
