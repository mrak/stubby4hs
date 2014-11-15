{-# LANGUAGE OverloadedStrings #-}
module Portal.LoggerMiddleware (logger) where
import Prelude hiding (putStr, putStrLn, concat)
import System.Console.ANSI
import Network.Wai
import Data.Time.LocalTime
import Data.Time.Format
import System.Locale
import Data.ByteString.Char8
import Network.HTTP.Types.Status

logger :: ByteString -> Middleware
logger name = logRequest name . logResponse name

logRequest :: ByteString -> Middleware
logRequest name app req sendResponse = do
    t1 <- getZonedTime
    let method = requestMethod req
        path   = rawPathInfo req
        ts1    = pack $ formatTime defaultTimeLocale "%T" t1
        msg1   = concat [ts1," -> ",method," ",name,path]
    colorStrLn Dull Cyan msg1
    app req sendResponse

logResponse :: ByteString -> Middleware
logResponse name app req sendResponse =
    app req $ \res -> do
        t2 <- getZonedTime
        let status = responseStatus res
            path   = rawPathInfo req
            code   = statusCode status
            color  = colorByStatus code
            reason = statusMessage status
            ts2    = pack $ formatTime defaultTimeLocale "%T" t2
            msg2   = concat [ts2," <- ",(pack . show) code," ",name,path," ",reason]
        colorStrLn Dull color msg2
        sendResponse res

colorStr :: ColorIntensity -> Color -> ByteString -> IO ()
colorStr fgi fg s = do
    setSGR [SetColor Foreground fgi fg]
    putStr s
    setSGR []

colorStrLn :: ColorIntensity -> Color -> ByteString -> IO ()
colorStrLn fgi fg s = colorStr fgi fg s >> putStrLn ""

colorByStatus :: Int -> Color
colorByStatus code
    | code < 200 = Blue
    | code < 300 = Green
    | code < 400 = Yellow
    | otherwise  = Red
