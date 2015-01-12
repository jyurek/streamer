{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Resource
import Data.ByteString as B hiding (foldl, length)
import Data.ByteString.Lazy as BL hiding (foldl, length)
import Data.ByteString.Char8 as C8 hiding (foldl, length)
import Data.Conduit
import Data.Conduit.Binary
import Network.HTTP.Types
import Network.Multipart
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = run 12345 app

app :: Application
app req respond = do
    print req
    body <- lazyRequestBody req
    let boundary  = boundaryHeader $ requestHeaders req
        bodyParts = parseMultipartBody (C8.unpack boundary) body
        parts     = partsFrom bodyParts
    print parts
    respond $ responseLBS status200 [] "Hello World"

boundaryHeader :: RequestHeaders -> B.ByteString
boundaryHeader ((h, ct):hs)
    | h == "Content-Type" = C8.dropWhile (=='=') $ C8.dropWhile (/='=') ct
    | otherwise           = boundaryHeader hs

partsFrom :: MultiPart -> [BodyPart]
partsFrom (MultiPart a) = a

-- outputPart :: BodyPart -> Int -> Int
-- outputPart part x =
--     runResourceT $ do
--         let source = sourceLbs part
--             sink = sinkFile "file" ++ show x ++ ".out"
--         source $$ sink
