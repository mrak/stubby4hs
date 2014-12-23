{-# LANGUAGE OverloadedStrings #-}
module Stubby.Data.Common
     ( parseHeaders
     ) where
import Data.Yaml
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Types (ResponseHeaders)
import qualified Data.HashMap.Strict as HM
import Control.Monad (mzero)
import Data.CaseInsensitive

parseHeaders :: Object -> Parser ResponseHeaders
parseHeaders o = case HM.lookup "headers" o
                 of   Nothing         -> return []
                      Just (Object h) -> return $ HM.foldrWithKey parseHeader [] h
                      _               -> mzero

parseHeader :: Text -> Value -> ResponseHeaders -> ResponseHeaders
parseHeader k v a = case v
                    of   (String s) -> (mk (encodeUtf8 k), encodeUtf8 s):a
                         _          -> a
