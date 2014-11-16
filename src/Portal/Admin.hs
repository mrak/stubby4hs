{-# LANGUAGE OverloadedStrings #-}
module Portal.Admin (adminserver) where
import CLI.Arguments (Arguments(..))
import CLI.Logging (status)
import Portal.LoggerMiddleware (logger)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Char8 as BS

adminserver' :: Application
adminserver' _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World! -- admin"

adminserver :: Arguments -> IO ()
adminserver args = do
    let msg = BS.concat [ "Admin"
                        , " portal running at http://"
                        , BS.pack $ location args
                        , ":"
                        , BS.pack $ show (admin args)
                        ]
    status msg
    run (admin args) $ logger args "admin" adminserver'
