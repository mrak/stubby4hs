{-# LANGUAGE OverloadedStrings #-}
module Stubby
     ( stubby
     , Stubby
     , getAdmin
     , getStubs
     , wait
     ) where

import Stubby.Settings (Settings, getQuiet, getDatafile)
import Stubby.CLI.Logging (info,stored)
import Stubby.Net.Admin (adminserver)
import Stubby.Net.Stubs (stubserver)
import Stubby.Data.Endpoint (Endpoint, getRequest)
import Stubby.Data.Request (getUrl, getMethods)

import qualified Data.ByteString.Char8 as BS (readFile, pack, concat)
import Data.Yaml (decode)
import Data.Text.Encoding (encodeUtf8)

import Control.Monad (unless)
import Control.Concurrent.Async (Async, async, waitEither_)
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError)

stubby :: Settings -> IO Stubby
stubby settings = do
    endpoints <- parseEndpoints (getDatafile settings)
    unless (getQuiet settings) $ startupMessages endpoints

    admin <- async $ adminserver settings
    stubs <- async $ stubserver settings
    return $ Stubby admin stubs

data Stubby = Stubby
    { adminThread :: Async ()
    , stubsThread :: Async ()
    }

getAdmin :: Stubby -> Async ()
getAdmin = adminThread

getStubs :: Stubby -> Async ()
getStubs = stubsThread

wait :: Stubby -> IO ()
wait s = waitEither_ (getAdmin s) (getStubs s)

parseEndpoints :: FilePath -> IO [Endpoint]
parseEndpoints f =  do
    c <- catch (BS.readFile f) readHandler
    case decode c :: Maybe [Endpoint] of
         Just a  -> return a
         Nothing -> error "Cannot parse data file"
    where readHandler e | isDoesNotExistError e = return "[]"
                        | otherwise             = ioError e

printLoaded :: [Endpoint] -> IO ()
printLoaded = mapM_ f
    where f e = let r = getRequest e
                    u = encodeUtf8 $ getUrl r
                    m = BS.pack $ show $ getMethods r
                in  stored $ BS.concat ["Loaded ",m," ",u]

startupMessages :: [Endpoint] -> IO ()
startupMessages es = printLoaded es >> info "\nQuit: ctrl-c\n"
