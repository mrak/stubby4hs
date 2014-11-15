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

colorStr :: ColorIntensity -> Color -> ColorIntensity -> Color -> ByteString -> IO ()
colorStr fgi fg bgi bg s = do
    setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]
    putStr s
    setSGR []

colorStrLn :: ColorIntensity -> Color -> ColorIntensity -> Color -> ByteString -> IO ()
colorStrLn fgi fg bgi bg s = colorStr fgi fg bgi bg s >> putStrLn ""

logger :: ByteString -> Middleware
logger name app req sendResponse = do
    t <- getZonedTime
    let method = requestMethod req
        path   = rawPathInfo req
        time   = pack $ formatTime defaultTimeLocale "%T" t
        msg    = concat [time," -> ",method," ",name,path]
    liftIO $ colorStrLn Dull Cyan Dull Black msg
    app req sendResponse
