{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Stubby.Data.Request (Request(..)) where
import Data.Text
import Data.Text.Encoding
import Data.Yaml
import Network.HTTP.Types
import GHC.Generics
import Data.ByteString.Char8 as BS
import Control.Applicative

data Request = Request
    { url    :: Text
    , method :: Method
    } deriving (Show,Generic)

instance FromJSON Request where
    parseJSON (Object o) = Request <$>
                           o .: "url" <*>
                           o .: "method"
    parseJSON _ = error "Request cannot be parsed"

instance FromJSON BS.ByteString where
    parseJSON (String s) = return $ encodeUtf8 s
    parseJSON _ = error "ByteString cannot be parsed"
