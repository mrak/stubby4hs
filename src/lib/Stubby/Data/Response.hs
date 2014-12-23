{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Stubby.Data.Response
     ( Response
     , defaultResponse
     , getStatus
     , getBody
     , getFile
     , getHeaders
     , getLatency
     ) where
import Stubby.Data.Common (parseHeaders)
import Data.Text (Text)
import Data.Yaml
import Network.HTTP.Types (Status, ok200, ResponseHeaders)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

data Response = Response
    { status  :: Status
    , body    :: Maybe Text
    , file    :: Maybe FilePath
    , latency :: Maybe Int
    , headers :: ResponseHeaders
    } deriving (Show)

defaultResponse :: Response
defaultResponse = Response
    { status  = ok200
    , body    = Nothing
    , file    = Nothing
    , latency = Nothing
    , headers = []
    }

getStatus :: Response -> Status
getStatus = status

getBody :: Response -> Maybe Text
getBody = body

getFile :: Response -> Maybe FilePath
getFile = file

getHeaders :: Response -> ResponseHeaders
getHeaders = headers

getLatency :: Response -> Maybe Int
getLatency = latency

instance FromJSON Response where
    parseJSON (Object o) = Response <$> o .:? "status" .!= ok200
                                    <*> o .:? "body"
                                    <*> o .:? "file"
                                    <*> o .:? "latency"
                                    <*> parseHeaders o
    parseJSON _ = mzero
