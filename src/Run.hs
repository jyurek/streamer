module Run where

import Accum

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.ByteString (ByteString)
import Data.Conduit
import Data.Conduit.Binary
import Data.UUID
import Data.UUID.V4
import Network.Wai (Request)
import Network.Wai.Conduit (sourceRequestBody)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

runRequest :: ByteString -> Request -> IO ()
runRequest boundary req = runResourceT $ do
    -- b <- sourceRequestBody req $$ sinkLbs
    -- liftIO $ BL.putStr b

    -- sourceRequestBody req $= takeUntil boundary $$ sinkPrint
    sendEach boundary (newResumableSource $ sourceRequestBody req) sinkPrint

saveFile :: MonadResource m => Sink ByteString m ()
saveFile = do
    fn <- liftIO $ fmap toString $ nextRandom
    sinkFile ("out/" ++ fn)

sinkPrint :: MonadResource m => Sink ByteString m ()
sinkPrint = do
    mv <- await
    case mv of
        Just v -> liftIO (B.putStr v) >> sinkPrint
        Nothing -> liftIO (putStrLn "") >> return ()
