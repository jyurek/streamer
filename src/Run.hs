{-# LANGUAGE OverloadedStrings #-}
module Run where

import Accum

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

runRequest :: ByteString -> Request -> IO ()
runRequest boundary req = runResourceT $ do
    sourceRequestBody req $= takeUntil boundary $$ multiSinkFile
    return ()

multiSinkFile :: MonadResource m => Sink (Progress, ByteString) m ()
multiSinkFile = multiSinkFile' newFile

multiSinkFile' :: MonadResource m => IO Handle -> Sink (Progress, ByteString) m ()
multiSinkFile' f = do
    mv <- await
    case mv of
        Nothing -> return ()
        Just (s, d) -> do
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
