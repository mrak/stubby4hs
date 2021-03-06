{-# LANGUAGE OverloadedStrings #-}
module Stubby.Data.Request
     ( Request
     , defaultRequest
     , getUrl
     , getMethods
     , getPost
     , getFile
     , getHeaders
     , getQuery
     ) where
import Stubby.Data.Common (parseHeaders)
import Network.HTTP.Types (Query, Method, RequestHeaders, methodGet)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml
import qualified Data.HashMap.Strict as HM (lookup, foldrWithKey)

data Request = Request
    { url     :: Text
    , methods :: [Method]
    , post    :: Maybe Text
    , file    :: Maybe FilePath
    , headers :: RequestHeaders
    , query   :: Query
    } deriving (Show)

defaultRequest :: Request
defaultRequest = Request
               { url     = "/"
               , methods = ["GET"]
               , post    = Nothing
               , file    = Nothing
               , headers = []
               , query   = []
               }

getUrl :: Request -> Text
getUrl = url

getMethods :: Request -> [Method]
getMethods = methods

getPost :: Request -> Maybe Text
getPost = post

getFile :: Request -> Maybe FilePath
getFile = file

getHeaders :: Request -> RequestHeaders
getHeaders = headers

getQuery :: Request -> Query
getQuery = query

instance FromJSON Request where
    parseJSON (Object o) = Request <$> o .:  "url"
                                   <*> parseMethods o
                                   <*> o .:? "file"
                                   <*> o .:? "post"
                                   <*> parseHeaders o
                                   <*> parseQuery o
    parseJSON _ = mzero

parseMethods :: Object -> Parser [Method]
parseMethods o = case HM.lookup "method" o
                 of   Nothing           -> return [methodGet]
                      Just (String s)   -> return [encodeUtf8 s]
                      Just as@(Array _) -> parseJSON as
                      _                 -> mzero

parseQuery :: Object -> Parser Query
parseQuery o = case HM.lookup "query" o
               of   Nothing         -> return []
                    Just (Object h) -> return $ HM.foldrWithKey parseQueryParam [] h
                    _               -> mzero

parseQueryParam :: Text -> Value -> Query -> Query
parseQueryParam k v a = case v
                        of   (String s) -> (encodeUtf8 k, Just (encodeUtf8 s)):a
                             Null       -> (encodeUtf8 k, Nothing):a
                             _          -> a
