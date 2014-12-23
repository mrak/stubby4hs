{-# LANGUAGE OverloadedStrings #-}
module Stubby.Data.Endpoint
     ( Endpoint
     , defaultEndpoint
     , getRequest
     , getResponses
     ) where
import Stubby.Data.Request (Request, defaultRequest)
import Stubby.Data.Response (Response, defaultResponse)
import Data.Yaml
import qualified Data.HashMap.Strict as HM (lookup)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)

data Endpoint = Endpoint
   { request :: Request
   , responses :: [Response]
   } deriving (Show)

defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint
                  { request = defaultRequest
                  , responses = [defaultResponse]
                  }

getRequest :: Endpoint -> Request
getRequest = request

getResponses :: Endpoint -> [Response]
getResponses = responses

instance FromJSON Endpoint where
    parseJSON (Object o) = Endpoint <$> o .: "request"
                                    <*> parseResponses o
    parseJSON _ = mzero


parseResponses :: Object -> Parser [Response]
parseResponses o = case HM.lookup "response" o
                   of   Nothing            -> return [defaultResponse]
                        Just as@(Array _)  -> parseJSON as
                        Just ro@(Object _) -> pure <$> parseJSON ro
                        _                  -> mzero
