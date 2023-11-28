{-# LANGUAGE OverloadedStrings #-}
module DHT.Test.BucketSpec where

import DHT.Core.Bucket
import DHT.Core.Types
import DHT.Core.Address
import DHT.Core.Contact
import DHT.Core.ID

import Test.Hspec

import Data.Maybe

epoch :: Time
epoch = 0

contact0ID :: ID
contact0ID = mkID (0 :: Int) 8

contact0Addr :: Address
contact0Addr = (IPV4 "127.0.0.1") `AddressThen` (Address $ UDP 6540)

contact1ID :: ID
contact1ID = mkID (1 :: Int) 8

contact1Addr :: Address
contact1Addr = (IPV4 "127.0.0.1") `AddressThen` (Address $ UDP 6541)

spec
  :: Spec
spec = do
  describe "A Bucket" $ do
    it "has no contacts after construction" $
      (contacts $ emptyBucket epoch)
        `shouldBe` []

    it "was last used at construction, after construction" $
      (lastUsed $ emptyBucket epoch)
        `shouldBe` epoch

    it "has a size equal to the number of unique Contacts" $ do
      (bucketSize $ emptyBucket epoch)
        `shouldBe` 0

      (bucketSize $ enter contact0ID contact0Addr (epoch+1)
                  $ emptyBucket epoch)
        `shouldBe` 1

      (bucketSize $ enter contact1ID contact1Addr (epoch+2)
                  $ enter contact0ID contact0Addr (epoch+1)
                  $ emptyBucket epoch)
        `shouldBe` 2

      (bucketSize $ enter contact1ID contact1Addr (epoch+2)
                  $ enter contact1ID contact0Addr (epoch+1)
                  $ emptyBucket epoch)
        `shouldBe` 1

    it "updates it's last used" $
      (lastUsed $ useBucket (epoch+1) $ emptyBucket epoch)
        `shouldBe` (epoch+1)

    -- Note: We don't use == here since contacts are considered equal by ID by
    -- default
    it "has a backdoor, allowing contacts to be modified" $ do
      (fmap contacts . modifyBucketContact contact0ID setBad
                     . enter contact1ID contact1Addr (epoch+2)
                     . enter contact0ID contact0Addr (epoch+1)
                     . emptyBucket
                     $ epoch)
        `shouldSatisfy` ( and
                        . zipWith exactlyEqual [ replaceID contact1ID . mkContact 8 $ contact1Addr
                                               , setBad . replaceID contact0ID . mkContact 8 $ contact0Addr
                                               ]
                        . fromJust)

    it "drops a single bad contact of it's choosing" $ do
      ( bucketSize
       . fromJust
       . dropBad
       . fromJust
       . modifyBucketContact contact1ID setBad
       . fromJust
       . modifyBucketContact contact0ID setBad
       . enter contact1ID contact1Addr (epoch+2)
       . enter contact0ID contact0Addr (epoch+1)
       . emptyBucket
       $ epoch)
        `shouldBe` 1

    -- Waay too complex
    it "updates Questionable contacts to Good" $ do
      ( fromJust
       . (`updateBucket` (\_ -> Just True))
       . fromJust
       . modifyBucketContact contact1ID setBad
       . fromJust
       . modifyBucketContact contact0ID setQuestionable
       . enter contact1ID contact1Addr (epoch+2)
       . enter contact0ID contact0Addr (epoch+1)
       . emptyBucket
       $ epoch)
        `shouldSatisfy` (\(bucket,False)
                          -> and
                              . zipWith exactlyEqual [setBad  . replaceID contact1ID . mkContact 8 $ contact1Addr
                                                     ,setGood . replaceID contact0ID . mkContact 8 $ contact0Addr
                                                     ]
                              . contacts
                              $ bucket
                         )

    it "updates Questionable contacts to Bad" $ do
      ( fromJust
       . (`updateBucket` (\_ -> Just False))
       . fromJust
       . modifyBucketContact contact1ID setBad
       . fromJust
       . modifyBucketContact contact0ID setQuestionable
       . enter contact1ID contact1Addr (epoch+2)
       . enter contact0ID contact0Addr (epoch+1)
       . emptyBucket
       $ epoch)
        `shouldSatisfy` (\(bucket,True)
                          -> and
                              . zipWith exactlyEqual [setBad  . replaceID contact1ID . mkContact 8 $ contact1Addr
                                                     ]
                              . contacts
                              $ bucket
                         )

    -- TODO: More thorough testing of splits
    -- - Each Contact must still be present
    -- - No extra Contacts must be present
    -- - Balance must be as expected
    it "splits in two across an ID" $ do
      let cs :: [Contact]
          cs = map (\i -> mkContact 8 $ (IPV4 "127.0.0.1") `AddressThen` (Address $ UDP i)) [1..10]

          b :: Bucket
          b = foldr (\contact bucket -> enter (contactID contact) (contactAddress contact) (epoch + 1) bucket)
                    (emptyBucket epoch)
                    cs
          (leftBucket,rightBucket) = split (mkID (100::Int) 8) 4 b

      (bucketSize leftBucket + bucketSize rightBucket) `shouldBe` 10

      -- TODO: These properties are not guaranteed to be true, but as the number
      -- of contacts increases this is more likely to catch a split failure
      -- It might be better to assert specific balances
      (bucketSize leftBucket)  `shouldNotBe` 0
      (bucketSize rightBucket) `shouldNotBe` 0

