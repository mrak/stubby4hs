{-# LANGUAGE OverloadedStrings #-}
module Portal.LoggerMiddleware (logger) where
import Prelude hiding (putStr, putStrLn, concat)
import CLI.Arguments
import System.Console.ANSI
import Network.Wai
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.ByteString.Char8
import Network.HTTP.Types.Status (statusCode, statusMessage)
import Control.Monad (unless)

logger :: Arguments -> ByteString -> Middleware
logger args name = logRequest args name . logResponse args name

logRequest :: Arguments -> ByteString -> Middleware
logRequest (Arguments _ _ q _ _) name app req sendResponse = do
    unless q $ do
        t1 <- getZonedTime
        let method = requestMethod req
            path   = rawPathInfo req
            ts1    = pack $ formatTime defaultTimeLocale "%T" t1
            msg1   = concat [ts1," -> ",method," ",name,path]
        colorStrLn Dull Cyan msg1
    app req sendResponse

logResponse :: Arguments -> ByteString -> Middleware
logResponse (Arguments _ _ q _ _) name app req sendResponse =
    app req $ \res -> do
        unless q $ do
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
    setSGR [Reset]

colorStrLn :: ColorIntensity -> Color -> ByteString -> IO ()
colorStrLn fgi fg s = colorStr fgi fg s >> putStrLn ""

colorByStatus :: Int -> Color
colorByStatus code
    | code < 200 = Blue
    | code < 300 = Green
    | code < 400 = Yellow
    | otherwise  = Red
