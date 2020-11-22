module DHT.Test.BitsSpec where

import DHT.Core.Bits

import Test.Hspec

spec
  :: Spec
spec = do
  describe "A bit" $ do
    it "Is unique" $ do
      Zero `shouldNotBe` One
      One  `shouldNotBe` Zero

    it "Is equal to itself" $ do
      Zero `shouldBe` Zero
      One  `shouldBe` One

    it "Has ordering" $ do
      (Zero <= One) `shouldBe` True
      (Zero <  One) `shouldBe` True
      (Zero >  One) `shouldBe` False
      (Zero >= One) `shouldBe` False

      (One <= Zero) `shouldBe` False
      (One <  Zero) `shouldBe` False
      (One >  Zero) `shouldBe` True
      (One >= One)  `shouldBe` True

    it "Can be xor'd" $ do
      Zero `xor` Zero `shouldBe` Zero
      Zero `xor` One  `shouldBe` One
      One  `xor` Zero `shouldBe` One
      One  `xor` One  `shouldBe` Zero

    it "Can be shown" $ do
      (show Zero) `shouldBe` "0"
      (show One)  `shouldBe` "1"

  describe "Bits" $ do
    it "Can be constructed from ints and truncated or padded to a length" $ do
      (show $ toBits 100 4)
        `shouldBe` "0100"

      (show $ toBits 100 8)
        `shouldBe` "01100100"

      (show $ toBits 100 16)
        `shouldBe` "0000000001100100"

      (show $ toBits 100 64)
        `shouldBe` "0000000000000000000000000000000000000000000000000000000001100100"

    it "Roundtrip through fromBits (assuming no overflow)" $ do
      100 `shouldBe` (fromBits . toBits 100 $ 8)
      100 `shouldBe` (fromBits . toBits 100 $ 32)

    it "Are not equal with leading padding 0's" $ do
      --          1100100
      -- 0000000001100100
      toBits 100 7
        `shouldNotBe` toBits 100 16

    it "Are equal to themselves" $ do
      toBits 100 8
        `shouldBe` toBits 100 8

    it "Can have their leading bit indexed" $ do
      -- 1100100
      (leadingBit . toBits 100 $ 7)
        `shouldBe` One

      -- 0100
      (leadingBit . toBits 100 $ 4)
        `shouldBe` Zero

    it "Can have leading bits dropped" $ do
      (dropLeadingBits 3 . toBits 100 $ 7)
        `shouldBe` (toBits 100 4)

    it "Have a length" $ do
      (lengthBits . toBits 100 $ 8)
        `shouldBe` 8

