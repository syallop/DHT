module DHT.Test.AddressSpec where

import DHT.Address
import Data.Hashable

import Test.Hspec

testAddress :: Address
testAddress = (IPV4 "127.0.0.1") `AddressThen` (Address $ UDP 6543)

spec
  :: Spec
spec = do
  describe "Addresses" $ do
    it "Can be constructed from parts" $
      (fromParts (IPV4 "127.0.0.1") [UDP 6543])
        `shouldBe` testAddress

    it "Can be deconstructed into their parts" $
      (parts testAddress)
        `shouldBe` [IPV4 "127.0.0.1", UDP 6543]

    it "Can be hashed to a stable value" $
      hash testAddress `shouldBe` (-6056951320977809105)

