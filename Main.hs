{-# LANGUAGE OverloadedStrings #-}
module Main where

import Run

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8 (dropWhile)
import Data.Monoid ((<>))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = run 12345 app

app :: Application
app req respond = do
    let headers = requestHeaders req
        Just boundary = extractBoundary headers
    print boundary
    runRequest boundary req
    respond $ responseLBS status200 [] "OK\n"

extractBoundary :: RequestHeaders -> Maybe B.ByteString
extractBoundary [] = Nothing
extractBoundary ((hn, hv):hs)
    | hn == "Content-Type" = Just $ ("--" <>) . C8.dropWhile (=='=') . C8.dropWhile (/='=') $ hv
    | otherwise = extractBoundary hs
