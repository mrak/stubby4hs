{-# LANGUAGE OverloadedStrings #-}
module Portal.LoggerMiddleware (logger) where
import Prelude hiding (error,putStr,putStrLn,concat)
import CLI.Arguments (Arguments(..))
import CLI.Logging (incoming,info,ok,warn,error)
import Network.Wai
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.ByteString.Char8
import Network.HTTP.Types.Status (statusCode,statusMessage)
import Control.Monad (unless)

logger :: Arguments -> ByteString -> Middleware
logger args name = logRequest args name . logResponse args name

logRequest :: Arguments -> ByteString -> Middleware
logRequest args name app req sendResponse = do
    unless (quiet args) $ do
        t1 <- getZonedTime
        let method = requestMethod req
            path   = rawPathInfo req
            ts1    = pack $ formatTime defaultTimeLocale "%T" t1
            msg1   = concat [ts1," -> ",method," ",name,path]
        incoming msg1
    app req sendResponse

logResponse :: Arguments -> ByteString -> Middleware
logResponse args name app req sendResponse =
    app req $ \res -> do
        unless (quiet args) $ do
            t2 <- getZonedTime
            let status = responseStatus res
                code   = statusCode status
                path   = rawPathInfo req
                reason = statusMessage status
                ts2    = pack $ formatTime defaultTimeLocale "%T" t2
                msg2   = concat [ts2," <- ",(pack . show) code," ",name,path," ",reason]
            logByStatus code msg2
        sendResponse res

logByStatus :: Int -> ByteString -> IO ()
logByStatus code
    | code < 200 = info
    | code < 300 = ok
    | code < 400 = warn
    | otherwise  = error
