{-# LANGUAGE OverloadedStrings #-}
module Test where

import Data.ByteString (ByteString)
import Data.Conduit

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

import System.IO

main :: IO ()
main = do
    lbs <- CB.sourceHandle stdin $$ CB.sinkLbs
    BL.putStrLn lbs
