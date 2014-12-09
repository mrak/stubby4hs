{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Stubby.Data.Request
     ( Request
     , defaultRequest
     , getUrl
     , getMethods
     , getPost
     , getFile
     ) where
import Prelude as P
import Network.HTTP.Types
import Control.Applicative
import Control.Monad (mzero)
import Data.Text
import Data.Text.Encoding
import Data.Yaml
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS

data Request = Request
    { url     :: Text
    , methods :: [Method]
    , post    :: Maybe Text
    , file    :: Maybe FilePath
    } deriving (Show)

defaultRequest :: Request
defaultRequest = Request
               { url = "/"
               , methods = ["GET"]
               , post = Nothing
               , file = Nothing
               }

getUrl :: Request -> Text
getUrl = url

getMethods :: Request -> [Method]
getMethods = methods

getPost :: Request -> Maybe Text
getPost = post

getFile :: Request -> Maybe FilePath
getFile = file

instance FromJSON Request where
    parseJSON (Object o) = Request <$> o .:  "url"
                                   <*> parseMethods o
                                   <*> o .:? "file"
                                   <*> o .:? "post"
    parseJSON _ = mzero

parseMethods :: Object -> Parser [Method]
parseMethods o = case HM.lookup "method" o
                 of   Nothing           -> return [methodGet]
                      Just (String s)   -> return [encodeUtf8 s]
                      Just as@(Array _) -> parseJSON as
                      _                 -> mzero

instance FromJSON BS.ByteString where
    parseJSON (String s) = return $ encodeUtf8 s
    parseJSON _ = mzero
