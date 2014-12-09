{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Stubby.Data.Response
     ( Response
     , defaultResponse
     , getStatus
     , getBody
     , getFile
     ) where
import Data.Text
import Data.Yaml
import Network.HTTP.Types (Status, ok200)
import Control.Applicative
import Data.Scientific
import Control.Monad (mzero)

data Response = Response
    { status :: Status
    , body   :: Maybe Text
    , file   :: Maybe FilePath
    } deriving (Show)

defaultResponse :: Response
defaultResponse = Response
    { status = ok200
    , body = Nothing
    , file = Nothing
    }

getStatus :: Response -> Status
getStatus = status

getBody :: Response -> Maybe Text
getBody = body

getFile :: Response -> Maybe FilePath
getFile = file

instance FromJSON Response where
    parseJSON (Object o) = Response <$> o .:? "status" .!= ok200
                                    <*> o .:? "body"
                                    <*> o .:? "file"
    parseJSON _ = mzero

instance FromJSON Status where
    parseJSON (Number n) = case toBoundedInteger n of
                                Nothing -> mzero
                                Just i -> parseStatus i
    parseJSON _ = mzero

parseStatus :: Int -> Parser Status
parseStatus s = return $ toEnum s
