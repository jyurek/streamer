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

data Progress = Continue | EndSection | EndContent

takeUntil :: MonadIO m
          => ByteString
          -> Conduit ByteString m (Progress, ByteString)
takeUntil boundary = do
    let bl = B.length boundary
    lbs <- CB.take $ bl * 3
    let bs = toStrict lbs

    if B.null bs
        then return ()
        else do
            isOpen' <- isOpen
            if isOpen'
                then case B.breakSubstring boundary bs of
                    (beginning, ending) -> do
                        leftover ending
                        yield (EndSection, beginning)
                        takeUntil boundary
                    otherwise -> do
                        leftover $ B.drop (bl * 2) bs
                        yield $ (Continue, B.take (bl * 2) bs)
                        takeUntil boundary
                else yield (EndContent, bs) >> return ()

    -- try this:
    -- take 3x boundary length
    -- if empty
    --   return
    -- if less than expected length or we can tell we're at the end of the stream
    --   yield it all
    -- if needle exists
    --   then leftover snd, yield fst
    --   else leftover (length of boundary), yield rest

    -- case () of
    --     _ | BL.null bs -> return ()
    --       | toStrict bs == boundary -> return ()
    --       | otherwise -> do
    --           yield $ toStrict $ BL.singleton $ BL.head bs
    --           leftover $ toStrict $ BL.tail bs

    --           takeUntil boundary

-- sendEach :: MonadIO m
--          => ByteString
--          -> ResumableSource m ByteString
--          -> Sink ByteString m ()
--          -> m ()
-- sendEach boundary source sink =  do
--     (s, b) <- source $$++ takeUntil boundary =$ sink

    -- if b == "--"
    --     then return ()
    --     else sendEach boundary s sink

    -- d <- isDone source

    -- if d
    --     then return ()
    --     else do
    --       (s, _) <- source $$++ takeUntil boundary =$ sink

    --       sendEach boundary s sink

  -- where
    -- isDone rs = do
    --     (s, _) <- unwrapResumable rs

    --     runConduit $ s $= CC.null

toStrict :: BL.ByteString -> ByteString
toStrict = B.concat . BL.toChunks

isOpen :: (MonadIO m) => ConduitM i o m Bool
isOpen = await >>= maybe (return False) ((True <$) . leftover)
-- isOpen = CL.peek >>= maybe (return False) (return True)
