{-# LANGUAGE OverloadedStrings #-}
module Run where

import Accum

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w)
import Data.Conduit
import Data.Conduit.Binary
import Data.UUID
import Data.UUID.V4
import Network.Wai (Request)
import Network.Wai.Conduit (sourceRequestBody)
import System.IO (openFile, hClose, Handle, IOMode(WriteMode))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

runRequest :: ByteString -> Request -> IO ()
runRequest boundary req = runResourceT $ do
    sourceRequestBody req $= takeUntil boundary $$ multiSinkFile
    return ()

    -- sourceRequestBody req $= removeVowels $$ sinkFile'
    -- b <- sourceRequestBody req $$ sinkLbs
    -- liftIO $ print boundary
    -- liftIO $ print "runRequest: "
    -- liftIO $ BL.putStr b
    -- let rsb = newResumableSource $ sourceRequestBody req

    -- (rsb', d) <- rsb $$++ takeUntil boundary =$ sinkPart
    -- liftIO $ print d
    -- liftIO $ print "-----------"
    -- (rsb'', d') <- rsb' $$++ consumePart boundary
    -- liftIO $ print d'
    -- liftIO $ print "-----------"
    -- (rsb''', d'') <- rsb'' $$++ consumePart boundary
    -- liftIO $ print d''
    -- liftIO $ print "-----------"

    -- forEachPart boundary rsb sinkFile'
    -- return ()

    -- sendEach boundary (newResumableSource $ sourceRequestBody req) sinkPrint

multiSinkFile :: MonadResource m => Sink (Progress, ByteString) m ()
multiSinkFile = multiSinkFile' newFile

multiSinkFile' :: MonadResource m => IO Handle -> Sink (Progress, ByteString) m ()
multiSinkFile' f = do
    mv <- await
    case mv of
        Nothing -> return ()
        Just (s, d) -> do
            liftIO $ print s
            case s of
                Continue -> do
                    liftIO $ iohPutStr f d
                    multiSinkFile' f
                EndSection -> do
                    liftIO $ iohPutStr f d
                    liftIO $ iohClose f
                    multiSinkFile' newFile
                EndContent -> do
                    liftIO $ iohPutStr f d
                    liftIO $ iohClose f

iohClose :: IO Handle -> IO ()
iohClose ioh = ioh >>= hClose

iohPutStr :: IO Handle -> ByteString -> IO ()
iohPutStr ioh s = ioh >>= flip B.hPutStr s

newFile :: IO Handle
newFile = do
    fn <- liftIO $ fmap toString $ nextRandom
    openFile ("out/" ++ fn) WriteMode


-- forEachPart :: MonadIO m
--                => ByteString
--                -> ResumableSource m ByteString
--                -> Sink ByteString m ()
--                -> m ()
-- forEachPart boundary source sink = do
--     (source', _) <- source $$++ takeUntil boundary =$ sink
--     forEachPart boundary source' sink

-- sinkFile' :: MonadResource m => Sink ByteString m ()
-- sinkFile' = do
--     fn <- liftIO $ fmap toString $ nextRandom
--     sinkFile ("out/" ++ fn)

-- sinkPrint :: MonadResource m => Sink ByteString m ()
-- sinkPrint = do
--     mv <- await
--     case mv of
--         Just v -> liftIO (B.putStr v) >> sinkPrint
--         Nothing -> liftIO (putStrLn "") >> return ()
