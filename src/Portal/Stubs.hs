{-# LANGUAGE OverloadedStrings #-}
module Portal.Stubs (stubserver) where
import CLI.Arguments (Arguments(..))
import CLI.Logging (status)
import Portal.LoggerMiddleware (logger)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Char8 as BS

stubserver' :: Application
stubserver' _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World! -- stubs"

stubserver :: Arguments -> IO ()
stubserver args = do
    let msg = BS.concat [ "Stubs"
                        , " portal running at http://"
                        , BS.pack $ location args
                        , ":"
                        , BS.pack $ show (stubs args)
                        ]
    status msg
    run (stubs args) $ logger args "stubs" stubserver'
