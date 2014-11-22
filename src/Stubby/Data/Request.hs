{-# LANGUAGE OverloadedStrings #-}
module Stubby.Data.Request (Request(..)) where
import Data.Text
import Data.Yaml
import Control.Applicative

data Request = Request
    { url    :: Text
    , method :: Text
    }

instance FromJSON Request where
    parseJSON (Object o) = Request <$>
                           o .: "url" <*>
                           o .: "method"
    parseJSON _ = error "Request error"
