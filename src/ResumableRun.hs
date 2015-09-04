{-# LANGUAGE OverloadedStrings #-}
module Streamer.ResumableRun where

runRequest :: ByteString -> Request -> IO ()
runRequest boundary req = runResourceT $ do
    newResumableSource $ sourceRequestBody req
