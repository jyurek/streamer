{-# LANGUAGE OverloadedStrings #-}
module Accum where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString (ByteString)
import Data.Conduit

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

data Progress = Continue | EndSection | EndContent deriving Show

takeUntil :: MonadIO m
          => ByteString
          -> Conduit ByteString m (Progress, ByteString)
takeUntil boundary = do
    let bl = B.length boundary
    lbs <- CB.take $ bl * 3
    let bs = toStrict lbs

    if B.null bs
        then liftIO $ print "END" >> return ()
        else do
            pv <- CL.peek
            case pv of
                Nothing -> yield (EndContent, bs) >> return ()
                Just _ -> do
                    let (before, after) = B.breakSubstring boundary bs
                    if B.null after
                        then do
                            leftover $ B.drop (bl * 2) bs
                            yield $ (Continue, B.take (bl * 2) bs)
                            takeUntil boundary
                        else do
                            leftover $ B.drop (bl+2) after
                            yield (EndSection, before)
                            takeUntil boundary

toStrict :: BL.ByteString -> ByteString
toStrict = B.concat . BL.toChunks

-- isOpen :: (MonadIO m) => ConduitM i o m Bool
-- isOpen = await >>= maybe (return False) ((True <$) . leftover)
-- -- isOpen = CL.peek >>= maybe (return False) (return True)
