{-# LANGUAGE OverloadedStrings #-}
module SplitSpec where

import Streamer.Split

import Test.Hspec

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.Conduit.Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C8

import qualified Data.Conduit.Binary as CB

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "takeUntil" $ do
        it "takes until the given boundary" $ do
            let input = sourceLbs $ C8.unlines
                    [ "---boundary---"
                    , "content 1"
                    , "content 1"
                    , "---boundary---"
                    , "content 2"
                    , "content 2"
                    , "---boundary---"
                    , "the rest"
                    , "---boundary---"
                    ]

            (input', b) <- input $$+ takeUntil "---boundary---\n"
            (input'', b') <- input' $$++ takeUntil "---boundary---\n"
            (input''', b'') <- input'' $$++ takeUntil "---boundary---\n"
            (input'''', b''') <- input''' $$++ takeUntil "---boundary---\n"
            (_, b'''') <- input'''' $$++ takeUntil "---boundary---\n"

            b `shouldBe` ""
            b' `shouldBe` "content 1\ncontent 1\n"
            b'' `shouldBe` "content 2\ncontent 2\n"
            b''' `shouldBe` "the rest\n"
            b'''' `shouldBe` ""
