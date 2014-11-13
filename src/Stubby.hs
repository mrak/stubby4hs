{-# LANGUAGE OverloadedStrings #-}
import Options
import Options.Applicative
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = execParser opts >>= stubby

stubby :: Options -> IO ()
stubby (Options _ s _ _ _) =
    run s stubserver

stubserver :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
stubserver _ respond = respond $
    responseLBS status200 [("Content-Type", "text/plain")] "Hello, World!"
