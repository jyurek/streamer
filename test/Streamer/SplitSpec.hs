{-# LANGUAGE OverloadedStrings #-}
module Streamer.SplitSpec where

import Streamer.Split

import Test.Hspec

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.Conduit.Binary
import Data.Conduit.List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8

import qualified Data.Conduit.Binary as CB

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "takeUntil" $ do
        it "takes until the given boundary" $ do
            let input = sourceLbs $ C8.intercalate "\r\n"
                    [ "---b---"
                    , "content 1"
                    , "content 1"
                    , "---b---"
                    , "content 2"
                    , "content 2"
                    , "---b---"
                    , "!234567890@234567890#234567890$234567890%234567890^234567890&234567890*234567890(234567890"
                    , "---b---"
                    , "the rest"
                    , "---b-----"
                    ]

            actual <- input $= takeUntil "---b---" $$ consume
            actual `shouldBe`
                    [ StartContent
                    , EndContent ""
                    , StartContent
                    , EndContent "content 1\r\ncontent 1\r\n"
                    , StartContent
                    , EndContent "content 2\r\ncontent 2\r\n"
                    , StartContent
                    , MoreContent "!234567890@234567890#234567890$234567890%234567890^234567890&23"
                    , EndContent "4567890*234567890(234567890\r\n"
                    , StartContent
                    , EndContent "the rest\r\n"
                    ]
