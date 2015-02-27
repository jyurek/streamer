{-# LANGUAGE OverloadedStrings #-}
module Accum where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Conduit

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as CC

takeUntil :: MonadIO m
          => ByteString
          -> Conduit ByteString m ByteString
takeUntil boundary = do
    bs <- CB.take $ B.length boundary

    liftIO $ do
        putStr "takeUntil: "
        BL.putStrLn bs

    case () of
        _ | BL.null bs -> return ()
          | toStrict bs == boundary -> return ()
          | otherwise -> do
              yield $ toStrict $ BL.singleton $ BL.head bs
              leftover $ toStrict $ BL.tail bs

              takeUntil boundary

sendEach :: MonadIO m
         => ByteString
         -> ResumableSource m ByteString
         -> Sink ByteString m ()
         -> m ()
sendEach boundary source sink =  do
    (s, b) <- source $$++ takeUntil boundary =$ sink

    if b == "--"
        then return ()
        else sendEach boundary s sink

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
