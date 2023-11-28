{-# LANGUAGE OverloadedStrings #-}
module DHT.Test.RoutingSpec where

import DHT.Core.Routing
import DHT.Core.ID
import DHT.Core.Types
import DHT.Core.Address
import DHT.Core.Bits
import DHT.Core.Contact

import Test.Hspec

import Data.Maybe
import Data.List (delete)
import Control.Monad.Writer (Writer, runWriter, execWriter, tell)

-- All localhost addresses on UDP, refered to as 127.0.0.1.
localhostAddresses
  :: [Address]
localhostAddresses = fmap (\portOffset -> (IPV4 "127.0.0.1") `AddressThen` (Address $ UDP portOffset)) [0..65535]

-- Used to mock monadic functions, accumulating the order Addresses were pinged (and whether they were good).
--
-- Note: Since this is an observable effect it's reasonable to test it's behaving
-- as expected. This is not quite the same as being able to violate the black-boxness
-- of a function and test it's implementation.
type MockM a = Writer [(Address,Bool)] a

pingAllGood :: Address -> MockM Bool
pingAllGood addr = tell [(addr,True)] >> pure True

pingAllBad :: Address -> MockM Bool
pingAllBad addr = tell [(addr,False)] >> pure False

pingFail :: Address -> MockM Bool
pingFail _ = error "Didn't expect ping callback to be called"

runMock :: MockM a -> (a, [(Address,Bool)])
runMock = runWriter

execMock :: MockM a -> [(Address,Bool)]
execMock = execWriter

spec
  :: Spec
spec = do
  let
      bucketSize :: Int
      bucketSize = 4

      idSize :: Int
      idSize = 8

      ourID :: ID
      ourID = mkID (1 :: Int) idSize

      epoch :: Time
      epoch = 0

      emptyRouting :: Routing
      emptyRouting = empty bucketSize ourID epoch

  describe "A Routing table begins empty" $ do
    describe "with fixed configuration for" $ do
      it "bucket size" $
        (maxBucketSize emptyRouting)
          `shouldBe` bucketSize

      it "our routing ID" $
        (ourRoutingID emptyRouting)
          `shouldBe` ourID

    it "with no contacts" $
      (toList emptyRouting)
        `shouldBe` []

    -- Should these be 0?
    it "with a depth of 1" $
      (depth emptyRouting)
        `shouldBe` 1
    it "with a near depth of 1" $
      (depthNear emptyRouting)
        `shouldBe` 1
    it "with a bucket count of one" $
      (bucketCount emptyRouting)
        `shouldBe` 1

  describe "Routing tables support inserting Contacts" $ do
    let
        -- Addresses with a different ID to us
        differentAddresses :: [Address]
        differentAddresses
          = map    (\(addr,_id)   -> addr)
          . filter (\(_addr, cId) -> cId /= ourID)
          . map    (\addr         -> (addr, mkID addr idSize))
          $ localhostAddresses

        -- An address with a different ID to us
        differentAddress :: Address
        differentAddress = head differentAddresses

        -- Addresses up to an index where each bit of the associated ID is
        -- considered 'Near'.
        nearAddresses :: Int -> [Address]
        nearAddresses level
          = map    (\(addr, _dist)  -> addr)
          . filter (\(_addr, dist)  -> all (== Near) . take level . _unBits . _unDistance $ dist)
          . map    (\addr           -> (addr, distance ourID . mkID addr $ idSize))
          $ localhostAddresses

        -- Addresses up to an index where each bit of the associated ID is
        -- considered 'Far'.
        farAddresses :: Int -> [Address]
        farAddresses level
          = map    (\(addr, _dist) -> addr)
          . filter (\(_addr, dist) -> all (== Far) . take level . _unBits . _unDistance $ dist)
          . map    (\addr          -> (addr, distance ourID . mkID addr $ idSize))
          $ localhostAddresses

    describe "when the Contacts ID is 'near'" $ do
      it "contacts will be inserted when empty" $ do

        (fst . runMock
             . insert differentAddress epoch pingFail idSize
             $ emptyRouting)
          `shouldSatisfy` (elem differentAddress . map contactAddress
                                                 . toList)

      it "contacts will always be inserted, even if it exceeds the max bucket size (the bucket will be split)" $ do
        let
            -- Addresses that will be near several buckets deep
            veryNearAddresses :: [Address]
            veryNearAddresses = nearAddresses 3

            -- Exactly enough Near addresses to fill a bucket
            bucketsWorthOfNearAddresses :: [Address]
            bucketsWorthOfNearAddresses = take bucketSize veryNearAddresses

            -- A single near address not already in the Routing table
            nearAddress :: Address
            nearAddress = head . take (bucketSize + 1) $ veryNearAddresses

            -- An initial routing table with exactly a bucket worth of Near
            -- addresses.
            initialRouting :: Routing
            initialRouting = fst . runMock . inserts bucketsWorthOfNearAddresses epoch pingFail idSize $ emptyRouting

            -- The intial routing table with a new near address inserted
            finalRouting :: Routing
            finalRouting = fst . runMock . insert nearAddress (epoch + 1) pingFail idSize $ initialRouting

            -- The Addresses contained in the final routing table
            finalAddresses :: [Address]
            finalAddresses = map contactAddress . toList $ finalRouting

        -- Double check we havn't breached the first bucket
        (bucketCount initialRouting)
          `shouldBe` 1

        -- The address we entered should be present
        finalAddresses
          `shouldSatisfy` (elem nearAddress)

        -- As should every initial address
        finalAddresses
          `shouldSatisfy` (\addrs -> and . map (`elem` addrs) $ bucketsWorthOfNearAddresses)

    describe "when the Contacts ID is 'far'" $ do
      let
          -- Addresses that will be near several buckets deep
          veryNearAddresses :: [Address]
          veryNearAddresses = nearAddresses 3

          -- Addresses that will be far several buckets deep
          veryFarAddresses :: [Address]
          veryFarAddresses = farAddresses 3

          -- Exactly enough Near addresses to fill a bucket
          bucketsWorthOfNearAddresses :: [Address]
          bucketsWorthOfNearAddresses = take bucketSize veryNearAddresses

          -- A single far address not already in the Routing table
          farAddress :: Address
          farAddress = head . take (bucketSize + 1) $ veryFarAddresses


      it "if all other contacts are good, it will not be inserted" $ do
        let
            -- An initial routing table with exactly a bucket worth of Near
            -- addresses.
            initialRouting :: Routing
            initialRouting = fst . runMock . inserts bucketsWorthOfNearAddresses epoch pingFail idSize $ emptyRouting

            -- The intial routing table with a new far address inserted
            finalRouting :: Routing
            finalRouting = fst . runMock . insert farAddress (epoch + 1) pingFail idSize $ initialRouting

            -- The Addresses contained in the final routing table
            finalAddresses :: [Address]
            finalAddresses = map contactAddress . toList $ finalRouting

        -- Double check we havn't breached the first bucket
        (bucketCount initialRouting)
          `shouldBe` 1

        -- The address we entered should not be present
        finalAddresses
          `shouldSatisfy` (not . elem farAddress)

        -- But all initial addresses should
        finalAddresses
          `shouldSatisfy` (\addrs -> and . map (`elem` addrs) $ bucketsWorthOfNearAddresses)

      it "if there is a bad Contact, it will be replaced" $ do
        let
            -- The Address to set to Bad
            badAddress :: Address
            badAddress = head bucketsWorthOfNearAddresses

            -- The ID t set to Bad
            badID :: ID
            badID = mkID badAddress idSize

            -- An initial routing table with exactly a bucket worth of Near
            -- addresses with one Contact set to Bad.
            initialRouting :: Routing
            initialRouting = fromJust . modify badID setBad
                                      . fst
                                      . runMock
                                      $ inserts bucketsWorthOfNearAddresses epoch pingFail idSize emptyRouting

            -- The intial routing table with a new far address inserted
            finalRouting :: Routing
            finalRouting = fst . runMock . insert farAddress (epoch + 1) pingFail idSize $ initialRouting

            -- The Addresses contained in the final routing table
            finalAddresses :: [Address]
            finalAddresses = map contactAddress . toList $ finalRouting

        -- Double check we havn't breached the first bucket
        (bucketCount initialRouting)
          `shouldBe` 1

        -- The address we entered should be present
        finalAddresses
          `shouldSatisfy` (elem farAddress)

        -- No bucket split should have occured, as a Bad Contact should have
        -- been replaced.
        (bucketCount finalRouting)
          `shouldBe` 1

        -- The Bad contact should no longer be present
        finalAddresses
          `shouldSatisfy` (all (not . (== badAddress)))

        -- The new address should be present
        finalAddresses
          `shouldSatisfy` (any (== farAddress))

        -- All prior Good addresses should remaining
        (delete badAddress bucketsWorthOfNearAddresses)
          `shouldSatisfy`
          (all (`elem` finalAddresses))

      describe "if there are Questionable contacts" $ do
        let
            -- The Address to set to Questionable
            questionableAddress :: Address
            questionableAddress = head bucketsWorthOfNearAddresses

            -- The ID to set to Questionable
            questionableID :: ID
            questionableID = mkID questionableAddress idSize

            -- An initial routing table with exactly a bucket worth of Near
            -- addresses with one Contact set to Questionable.
            initialRouting :: Routing
            initialRouting = fromJust . modify questionableID setQuestionable
                                      . fst
                                      . runMock
                                      $ inserts bucketsWorthOfNearAddresses epoch pingFail idSize emptyRouting


        it "they will be pinged" $ do
          let
               -- The intial routing table with a new far address inserted.
               -- All Questionable contacts will be ping'd to be Good.
               (finalRouting, pinged) = runMock . insert farAddress (epoch + 1) pingAllGood idSize $ initialRouting

               -- The Addresses contained in the final routing table
               finalAddresses :: [Address]
               finalAddresses = map contactAddress . toList $ finalRouting

          -- Double check we havn't breached the first bucket
          (bucketCount initialRouting)
            `shouldBe` 1

          -- The Questionable contact should have been pinged
          pinged
            `shouldSatisfy` (elem questionableAddress . map fst)

        it "if any Questionable Contact becomes Bad, it will be replaced" $ do
          let
               -- The intial routing table with a new far address inserted.
               -- All Questionable contacts will be ping'd to be Bad.
               (finalRouting, pinged) = runMock . insert farAddress (epoch + 1) pingAllBad idSize $ initialRouting

               -- The Addresses contained in the final routing table
               finalAddresses :: [Address]
               finalAddresses = map contactAddress . toList $ finalRouting

          -- Double check we havn't breached the first bucket
          (bucketCount initialRouting)
            `shouldBe` 1

          -- The Questionable contact should have been pinged
          pinged
            `shouldSatisfy` (elem (questionableAddress,False))

          -- No bucket split should have occured, as a Questionable->Bad Contact should have
          -- been replaced.
          (bucketCount finalRouting)
            `shouldBe` 1

          -- The Questionable->Bad contact should no longer be present
          finalAddresses
            `shouldSatisfy` (all (not . (== questionableAddress)))

          -- The new address should be present
          finalAddresses
            `shouldSatisfy` (any (== farAddress))

          -- All prior Good addresses should remaining
          (delete questionableAddress bucketsWorthOfNearAddresses)
            `shouldSatisfy`
            (all (`elem` finalAddresses))

        it "if no Questionable Contacts become Bad, the Contact will not be inserted" $ do
          let
               -- The intial routing table with a new far address inserted.
               -- All Questionable contacts will be ping'd to be Good.
               (finalRouting, pinged) = runMock . insert farAddress (epoch + 1) pingAllGood idSize $ initialRouting

               -- The Addresses contained in the final routing table
               finalAddresses :: [Address]
               finalAddresses = map contactAddress . toList $ finalRouting

          -- Double check we havn't breached the first bucket
          (bucketCount initialRouting)
            `shouldBe` 1

          -- The Questionable contact should have been pinged and become Good
          pinged
            `shouldSatisfy` (elem (questionableAddress,True))

          -- No bucket split should have occured.
          -- The Questionable Contact should have become Good.
          -- Since all Contacts are near, the proposed Far contact should not
          -- have been entered.
          (bucketCount finalRouting)
            `shouldBe` 1

          -- The Questionable->Good contact should be present
          finalAddresses
            `shouldSatisfy` (any (== questionableAddress))

          -- The new address should Not be present
          finalAddresses
            `shouldSatisfy` (all (not . (== farAddress)))

          -- All prior Good addresses should remaining
          (delete questionableAddress bucketsWorthOfNearAddresses)
            `shouldSatisfy`
            (all (`elem` finalAddresses))

