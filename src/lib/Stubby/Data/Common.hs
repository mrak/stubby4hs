{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Stubby.Data.Common
     ( parseHeaders
     ) where
import Data.Yaml
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Status, ResponseHeaders)
import Data.Scientific (toBoundedInteger)
import qualified Data.HashMap.Strict as HM (lookup, foldrWithKey)
import Control.Monad (mzero)
import Data.CaseInsensitive (mk)
import Data.ByteString.Char8 (ByteString)

parseHeaders :: Object -> Parser ResponseHeaders
parseHeaders o = case HM.lookup "headers" o
                 of   Nothing         -> return []
                      Just (Object h) -> return $ HM.foldrWithKey parseHeader [] h
                      _               -> mzero

parseHeader :: Text -> Value -> ResponseHeaders -> ResponseHeaders
parseHeader k v a = case v
                    of   (String s) -> (mk (encodeUtf8 k), encodeUtf8 s):a
                         _          -> a

instance FromJSON ByteString where
    parseJSON (String s) = return $ encodeUtf8 s
    parseJSON _ = mzero

instance FromJSON Status where
    parseJSON (Number n) = case toBoundedInteger n of
                                Nothing -> mzero
                                Just i -> parseStatus i
    parseJSON _ = mzero

parseStatus :: Int -> Parser Status
parseStatus s = return $ toEnum s
