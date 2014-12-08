{-# LANGUAGE OverloadedStrings #-}
module Stubby (
       stubby
     , Stubby
     , getAdmin
     , getStubs
     , wait
     ) where

import Stubby.Settings (Settings, getQuiet, getDatafile)
import Stubby.CLI.Logging (info,stored)
import Stubby.Net.Admin (adminserver)
import Stubby.Net.Stubs (stubserver)
import Stubby.Data.Endpoint
import Stubby.Data.Request

import qualified Data.ByteString.Char8 as BS
import Data.Yaml (decode)
import Data.Text.Encoding (encodeUtf8)

import Control.Monad (unless)
import Control.Concurrent.Async (Async, async, waitEither_)
import Control.Exception
import System.IO.Error

stubby :: Settings -> IO Stubby
stubby settings = do
    endpoints <- parseEndpoints (getDatafile settings)
    unless (getQuiet settings) $ printLoaded endpoints >> quitMessage

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
    where f e = let r = request e
                    u = encodeUtf8 $ url r
                    m = BS.pack $ show $ method r
                in  stored $ BS.concat ["Loaded ",m," ",u]

quitMessage :: IO ()
quitMessage = info "" >> info "Quit: ctrl-c" >> info ""
