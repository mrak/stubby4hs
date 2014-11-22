{-# LANGUAGE OverloadedStrings #-}
module Stubby.Data.Response (Response(..)) where
import Data.Text
import Data.Yaml
import Control.Applicative

data Response = Response
    { status :: Int
    , body   :: Text
    }

instance FromJSON Response where
    parseJSON (Object o) = Response <$>
                           o .: "status" <*>
                           o .: "body"
    parseJSON _ = error "Response error"
