{-# LANGUAGE OverloadedStrings #-}
module DHT.Test.IDSpec where

import DHT.Core.ID

import Test.Hspec

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Control.Exception (evaluate)
import Control.DeepSeq (force)

spec
  :: Spec
spec = do
  describe "An ID" $ do
    it "Can be constructed from hashable values" $ do
      (show $ mkID (100 :: Int) 8)
        `shouldBe` "01100100"

      (show $ mkID (100 :: Int) 8)
        `shouldBe` (show $ toBits 100 8)

      (show $ mkID () 2)
        `shouldBe` "00"

      (show $ mkID False 2)
        `shouldBe` "00"

      (show $ mkID True 2)
        `shouldBe` "01"

    it "Lazy and strict ByteStrings create the same IDs" $ do
      -- There's no guarantee bytestring doesn't change this behavior but it's
      -- nice to know whether it can be relied upon.
      (mkID ("words"::ByteString) 8)
        `shouldBe` (mkID ("words"::Lazy.ByteString) 8)

    it "Text, Strings and ByteStrings do NOT create the same IDs" $ do
      -- This is an easy mistake to make. This test calls out that it can't be
      -- expected rather than being a property we actually want...
      (mkID ("words"::Text) 8)
        `shouldNotBe` (mkID ("words"::String) 8)

      (mkID ("words"::Text) 8)
        `shouldNotBe` (mkID ("words"::ByteString) 8)

      (mkID ("words"::Text) 8)
        `shouldNotBe` (mkID ("words"::String) 8)

      (mkID ("words"::String) 8)
        `shouldNotBe` (mkID ("words"::ByteString) 8)

  describe "Distance" $ do
    it "Between an ID and itself should be all Zeros" $ do
      (show . _unDistance . distance (mkID (100 :: Int) 8) $ (mkID (100 :: Int) 8))
        `shouldBe` "00000000"

    it "Between ID's of different lengths is an exception" $ do
      (evaluate . force $ distance (mkID (100 :: Int) 8) (mkID (100 :: Int) 16))
        `shouldThrow` anyErrorCall

    it "Are as expected" $ do
      (show . _unDistance . distance (mkID (100 :: Int) 8) $
                                     (mkID (101 :: Int) 8))
        `shouldBe` "00000001"

      (show . _unDistance . distance (mkID (100 :: Int) 8) $
                                     (mkID (102 :: Int) 8))
        `shouldBe` "00000010"

      (show . _unDistance . distance (mkID (100 :: Int) 8) $
                                     (mkID (103 :: Int) 8))
        `shouldBe` "00000011"

      (show . _unDistance . distance (mkID (100 :: Int) 8) $
                                     (mkID (104 :: Int) 8))
        `shouldBe` "00001100"

      (show . _unDistance . distance (mkID (100 :: Int) 8) $
                                     (mkID (105 :: Int) 8))
        `shouldBe` "00001101"

      (show . _unDistance . distance (mkID (100 :: Int) 8) $
                                     (mkID (106 :: Int) 8))
        `shouldBe` "00001110"

      (show . _unDistance . distance (mkID (100 :: Int) 8) $
                                     (mkID (107 :: Int) 8))
        `shouldBe` "00001111"

      (show . _unDistance . distance (mkID (100 :: Int) 8) $
                                     (mkID (108 :: Int) 8))
        `shouldBe` "00001000"


