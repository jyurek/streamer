{-# LANGUAGE OverloadedStrings #-}
module Streamer.Split where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Conduit

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

data Progress = StartContent
              | MoreContent B.ByteString
              | EndContent B.ByteString
                  deriving (Show, Eq)

chunkSizes :: Int -> (Int, Int, Int)
chunkSizes x = (x, x*(multiplier-1), x*multiplier)
    where multiplier = 10

takeUntil :: MonadIO m
          => ByteString
          -> Conduit ByteString m Progress
takeUntil boundary = do
    takeUntil' boundary True

takeUntil' :: MonadIO m
           => ByteString
           -> Bool
           -> Conduit ByteString m Progress
takeUntil' boundary startNew = do
    let bl = B.length boundary
    let (boundarySize, keepSize, chunkSize) = chunkSizes(bl)
    lbs <- CB.take chunkSize
    let bs = toStrict lbs

    if B.null bs
        then return ()
        else do
            if startNew then yield StartContent
                        else return ()
            let (before, after) = B.breakSubstring boundary bs
            if B.null after
                then do
                    pv <- CL.peek
                    case pv of
                        Nothing -> do
                            yield $ EndContent bs
                            return ()
                        Just _ -> do
                            leftover $ B.drop keepSize bs
                            yield $ MoreContent $ B.take keepSize bs
                            takeUntil' boundary False
                else do
                    leftover $ B.drop (boundarySize+2) after
                    yield $ EndContent before
                    takeUntil' boundary True

toStrict :: BL.ByteString -> ByteString
toStrict = B.concat . BL.toChunks
