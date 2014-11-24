{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Stubby.Data.Response (Response(..)) where
import Data.Text
import Data.Yaml
import GHC.Generics
import Network.HTTP.Types
import Control.Applicative
import Data.Scientific
import Data.ByteString.Char8
import Control.Monad (mzero)

data Response = Response
    { status :: Status
    , body   :: Text
    } deriving (Show,Generic)

instance FromJSON Response where
    parseJSON (Object o) = Response <$> o .: "status"
                                    <*> o .: "body"
    parseJSON _ = mzero

instance FromJSON Status where
    parseJSON (Number n) = case toBoundedInteger n of
                                Nothing -> mzero
                                Just i -> parseStatus i
    parseJSON _ = mzero

parseStatus :: Int -> Parser Status
parseStatus n | n < 100 || n > 599  = mzero
              | otherwise = return $ Status n (getPhrase n)

getPhrase :: Int -> ByteString
getPhrase i | i == 100 = "Continue"
            | i == 101 = "Switching Protocols"
            | i == 102 = "Processing"

            | i == 200 = "OK"
            | i == 201 = "Created"
            | i == 202 = "Accepted"
            | i == 203 = "Non-Authoritative Information"
            | i == 204 = "No Content"
            | i == 205 = "Reset Content"
            | i == 206 = "Partial Content"
            | i == 207 = "Multi-Status"
            | i == 208 = "Already Reported"
            | i == 226 = "IM Used Reported"

            | i == 300 = "Multiple Choices"
            | i == 301 = "Moved Permanently"
            | i == 302 = "Found"
            | i == 303 = "See Other"
            | i == 304 = "Not Modified"
            | i == 305 = "Use Proxy"
            | i == 306 = "Switch Proxy"
            | i == 307 = "Temporary Redirect"
            | i == 308 = "Permanent Redirect"

            | i == 400 = "Bad Request"
            | i == 401 = "Unauthorized"
            | i == 402 = "Payment Required"
            | i == 403 = "Forbidden"
            | i == 404 = "Not Found"
            | i == 405 = "Method Not Allowed"
            | i == 406 = "Not Acceptable"
            | i == 407 = "Proxy Authentication Required"
            | i == 408 = "Request Timeout"
            | i == 409 = "Conflict"
            | i == 410 = "Gone"
            | i == 411 = "Length Required"
            | i == 412 = "Precondition Failed"
            | i == 413 = "Request Entity Too Large"
            | i == 414 = "Request-URI Too Long"
            | i == 415 = "Unsupported Media Type"
            | i == 416 = "Requested Range Not Satisfiable"
            | i == 417 = "Expectation Failed"
            | i == 418 = "I'm a teapot"
            | i == 422 = "Unprocessable Entity"
            | i == 423 = "Locked"
            | i == 424 = "Failed Dependency"
            | i == 426 = "Upgrade Required"
            | i == 428 = "Precondition Required"
            | i == 429 = "Too Many Requests"
            | i == 431 = "Request Header Fields Too Large"

            | i == 500 = "Internal Server Error"
            | i == 501 = "Not Implemented"
            | i == 502 = "Bad Gateway"
            | i == 503 = "Service Unavailable"
            | i == 504 = "Gateway Timeout"
            | i == 505 = "HTTP Version Not Supported"
            | i == 506 = "Variant Also Negotiates"
            | i == 507 = "Insufficient Storage"
            | i == 508 = "Loop Detected"
            | i == 510 = "Not Extended"
            | i == 511 = "Network Authentication Required"
            | otherwise = ""
