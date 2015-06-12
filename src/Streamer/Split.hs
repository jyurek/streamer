{-# LANGUAGE OverloadedStrings #-}
module Streamer.Split where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Conduit

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

data Progress = Continue ByteString
              | EndSection ByteString
              | EndContent ByteString
                  deriving Show

chunkMult :: Int
chunkMult = 10

takeUntil :: MonadIO m
          => ByteString
          -> Conduit ByteString m Progress
takeUntil boundary = do
    let bl = B.length boundary
    lbs <- CB.take $ bl * chunkMult
    let bs = toStrict lbs

    if B.null bs
        then return ()
        else do
            let (before, after) = B.breakSubstring boundary bs
            if B.null after
                then do
                    pv <- CL.peek
                    case pv of
                        Nothing -> do
                            yield $ EndContent bs
                            return ()
                        Just _ -> do
                            leftover $ B.drop (bl * (chunkMult-1)) bs
                            yield $ Continue $ B.take (bl * (chunkMult-1)) bs
                            takeUntil boundary
                else do
                    leftover $ B.drop (bl+2) after
                    yield $ EndSection before
                    takeUntil boundary

toStrict :: BL.ByteString -> ByteString
toStrict = B.concat . BL.toChunks

-- isOpen :: (MonadIO m) => ConduitM i o m Bool
-- isOpen = await >>= maybe (return False) ((True <$) . leftover)
-- -- isOpen = CL.peek >>= maybe (return False) (return True)
