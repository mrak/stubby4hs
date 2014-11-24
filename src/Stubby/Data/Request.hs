{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Stubby.Data.Request (Request(..)) where
import Prelude as P
import GHC.Generics
import Network.HTTP.Types
import Control.Applicative
import Control.Monad
import Data.Text
import Data.Text.Encoding
import Data.Yaml
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as BS

data Request = Request
    { url    :: Text
    , method :: [Method]
    , post   :: Maybe Text
    , file   :: Maybe FilePath
    } deriving (Show,Generic)

instance FromJSON Request where
    parseJSON (Object o) = Request <$> o .:  "url"
                                   {-<*> o .:? "method" .!= [methodGet]-}
                                   <*> parseMthd o
                                   <*> o .:? "file"
                                   <*> o .:? "post"
    parseJSON _ = mzero

parseMthd :: Object -> Parser [Method]
parseMthd o = case HM.lookup "method" o
              of   Nothing           -> return [methodGet]
                   Just (String s)   -> return [encodeUtf8 s]
                   Just as@(Array _) -> parseJSON as
                   _                 -> mzero

instance FromJSON BS.ByteString where
    parseJSON (String s) = return $ encodeUtf8 s
    parseJSON _ = mzero
