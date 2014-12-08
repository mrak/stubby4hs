{-# LANGUAGE OverloadedStrings #-}
module Stubby.Net.LoggerMiddleware (logger) where
import Prelude hiding (error,putStr,putStrLn,concat,dropWhile,tail)
import Stubby.Settings (Settings, getAdmin, getQuiet)
import qualified Stubby.CLI.Logging as L
import Network.Wai
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.ByteString.Char8
import Network.HTTP.Types.Status (statusCode,statusMessage)
import Control.Monad (unless)
import Control.Applicative

logger :: Settings -> Middleware
logger args = logRequest args . logResponse args

logRequest :: Settings -> Middleware
logRequest args app req sendResponse = do
    unless (getQuiet args) $ do
        t1 <- getZonedTime
        let method = requestMethod req
            name   = portalName args req
            path   = rawPathInfo req
            ts1    = pack $ formatTime defaultTimeLocale "%T" t1
            msg1   = concat [ts1," -> ",method," [",name,"]",path]
        L.incoming msg1
    app req sendResponse

logResponse :: Settings -> Middleware
logResponse args app req sendResponse =
    app req $ \res -> do
        unless (getQuiet args) $ do
            t2 <- getZonedTime
            let status = responseStatus res
                code   = statusCode status
                reason = statusMessage status
                name   = portalName args req
                path   = rawPathInfo req
                ts2    = pack $ formatTime defaultTimeLocale "%T" t2
                msg2   = concat [ts2," <- ",(pack . show) code," [",name,"]",path," ",reason]
            outgoing code msg2
        sendResponse res

portalName :: Settings -> Request -> ByteString
portalName args req =
    case isAdmin req of
        Just True -> "admin"
        _         -> "stubs"
    where isAdmin r = eqAdmin <$> getPort <$> requestHeaderHost r
          getPort   = tail <$> dropWhile (/= ':')
          eqAdmin   = (== (intToBS $ getAdmin args))
          intToBS   = pack . show

outgoing :: Int -> ByteString -> IO ()
outgoing code
    | code < 200 = L.info
    | code < 300 = L.ok
    | code < 400 = L.warn
    | otherwise  = L.error
