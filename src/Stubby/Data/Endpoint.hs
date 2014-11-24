{-# LANGUAGE OverloadedStrings #-}
module Stubby.Data.Endpoint (Endpoint(..)) where
import Stubby.Data.Request
import Stubby.Data.Response
import Data.Yaml
import Control.Applicative

data Endpoint = Endpoint
    { request :: Request
    , response :: Response
    }

instance FromJSON Endpoint where
    parseJSON (Object o) = Endpoint <$> o .: "request"
                                    <*> o .: "response"
    parseJSON _ = error "Endpoint error"

