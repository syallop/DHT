{-# LANGUAGE OverloadedStrings #-}
module DHT.Test.ContactSpec where

import DHT.Contact
import DHT.Address
import DHT.ID

import Test.Hspec

contact0Addr :: Address
contact0Addr = (IPV4 "127.0.0.1") `AddressThen` (Address $ UDP 6540)

spec
  :: Spec
spec = do
  describe "A Contact ID" $ do
    it "derives from it's address by default" $
      (show . contactID . mkContact 8 $ contact0Addr)
        `shouldBe` (show $ mkID contact0Addr 8)

    it "can be overidden (if you're careful what you're doing!)" $
      (contactID . replaceID (mkID (100::Int) 8)
                 . mkContact 8
                 $ contact0Addr)
        `shouldBe` (mkID (100::Int) 8)

  describe "A Contacts Goodness" $ do
    it "is initially Good" $
      (contactGoodness $ mkContact 8 contact0Addr)
        `shouldBe` Good

    it "can be modified" $ do
      (contactGoodness . setBad . mkContact 8 $ contact0Addr)
        `shouldBe` Bad

      (contactGoodness . setQuestionable. mkContact 8 $ contact0Addr)
        `shouldBe` Questionable

      (contactGoodness . setGood . mkContact 8 $ contact0Addr)
        `shouldBe` Good

      (contactGoodness . setBad . setQuestionable. mkContact 8 $ contact0Addr)
        `shouldBe` Bad

      (contactGoodness . setGood . setBad . mkContact 8 $ contact0Addr)
        `shouldBe` Good

  describe "Contacts can be compared for equality" $ do
    it "only considering ID's by default" $ do
      ((mkContact 8 $ contact0Addr) == (mkContact 8 contact0Addr))
        `shouldBe` True

      ((setQuestionable $ mkContact 8 $ contact0Addr) == (setBad $ mkContact 8 contact0Addr))
        `shouldBe` True

      (    (replaceID (mkID (100::Int) 8) $ mkContact 8 contact0Addr)
        == (replaceID (mkID (101::Int) 8) $ mkContact 8 contact0Addr))
        `shouldBe` False

