{-# LANGUAGE OverloadedStrings #-}
module Streamer.Run where

import Streamer.Split

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.ByteString (ByteString)
import Data.Conduit
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

multiSinkFile :: MonadResource m => Sink Progress m ()
multiSinkFile = liftIO newFile >>= multiSinkFile'

multiSinkFile' :: MonadResource m => Handle -> Sink Progress m ()
multiSinkFile' f = do
    mv <- await
    case mv of
        Nothing -> return ()
        Just p -> do
            case p of
                Continue d -> do
                    liftIO $ B.hPutStr f d
                    multiSinkFile' f
                EndSection d -> do
                    liftIO $ B.hPutStr f d
                    liftIO $ hClose f
                    f' <- liftIO newFile
                    multiSinkFile' f'
                EndContent d -> do
                    liftIO $ B.hPutStr f d
                    liftIO $ hClose f

newFile :: IO Handle
newFile = do
    fn <- liftIO $ fmap toString $ nextRandom
    openFile ("out/" ++ fn) WriteMode
