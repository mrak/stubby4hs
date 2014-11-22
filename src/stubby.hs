{-# LANGUAGE OverloadedStrings #-}
import Stubby.CLI.Arguments
import Stubby.CLI.Logging (info,stored)
import Stubby.Net.Admin (adminserver)
import Stubby.Net.Stubs (stubserver)
import Stubby.Data.Endpoint
import Stubby.Data.Request

import qualified Data.ByteString.Char8 as BS
import Data.Yaml (decode)
import Data.Text.Encoding (encodeUtf8)

import Control.Monad (unless)
import Options.Applicative (execParser)
import GHC.Conc.Sync
import Control.Exception
import System.IO.Error

main :: IO ()
main = do
    args <- execParser arguments
    endpoints <- parseEndpoints (datafile args)
    unless (quiet args) $ printLoaded endpoints >> quitMessage

    _ <- forkIO $ adminserver args
    stubserver args

parseEndpoints :: FilePath -> IO [Endpoint]
parseEndpoints f =  do
    c <- catch (BS.readFile f) readHandler
    case decode c :: Maybe [Endpoint] of
         Just a  -> return a
         Nothing -> return []
    where readHandler e | isDoesNotExistError e = return "[]"
                        | otherwise             = ioError e

printLoaded :: [Endpoint] -> IO ()
printLoaded es = mapM_ f es
    where f e = let r = request e
                    u = encodeUtf8 $ url r
                    m = method r
                in  stored $ BS.concat ["Loaded ",m," ",u]

quitMessage :: IO ()
quitMessage = info "" >> info "Quit: ctrl-c" >> info ""
