{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module CSGContractSpec where

import Test.Hspec
import Test.QuickCheck
import PlutusTx.Prelude
import Ledger
import Ledger.Ada as Ada
import Ledger.TimeSlot
import CSGContract

spec :: Spec
spec = do
  describe "CSG Contract" $ do
    it "should allow joining within the correct time frame" $ property $ \csg pkh amount ->
      let ctx = defaultContext { txInfoValidRange = interval (csgStartTime csg) (csgEndTime csg) }
      in validateJoin csg pkh amount ctx shouldBe True

    it "should not allow joining after the end time" $ property $ \csg pkh amount ->
      let ctx = defaultContext { txInfoValidRange = interval (csgEndTime csg + 1) (csgEndTime csg + 100) }
      in validateJoin csg pkh amount ctx shouldBe False

    it "should allow claiming rewards within the correct time frame" $ property $ \csg pkh ->
      let ctx = defaultContext { txInfoValidRange = interval (fromMilliSeconds $ 1000000 * 60 * 60 * 24 * 7 * (csgWeek csg - 1)) (fromMilliSeconds $ 1000000 * 60 * 60 * 24 * 7 * csgWeek csg) }
      in validateClaimReward csg pkh ctx shouldBe True

    it "should not allow claiming rewards outside the correct time frame" $ property $ \csg pkh ->
      let ctx = defaultContext { txInfoValidRange = interval (fromMilliSeconds $ 1000000 * 60 * 60 * 24 * 7 * csgWeek csg + 1) (fromMilliSeconds $ 1000000 * 60 * 60 * 24 * 7 * (csgWeek csg + 1)) }
      in validateClaimReward csg pkh ctx shouldBe False

    it "should allow ending the cycle after 52 weeks" $ property $ \csg ->
      let csg' = csg { csgWeek = 52 }
          ctx = defaultContext { txInfoValidRange = interval (csgEndTime csg') (csgEndTime csg' + 100) }
      in validateEndCycle csg' ctx shouldBe True

    it "should not allow ending the cycle before 52 weeks" $ property $ \csg ->
      let csg' = csg { csgWeek = 51 }
          ctx = defaultContext { txInfoValidRange = interval (csgEndTime csg') (csgEndTime csg' + 100) }
      in validateEndCycle csg' ctx shouldBe False

    it "should allow withdrawing after the CSG has ended" $ property $ \csg pkh amount ->
      let csg' = csg { csgActive = False }
      in validateWithdraw csg' pkh amount defaultContext shouldBe True

    it "should not allow withdrawing while the CSG is active" $ property $ \csg pkh amount ->
      let csg' = csg { csgActive = True }
      in validateWithdraw csg' pkh amount defaultContext shouldBe False

defaultContext :: ScriptContext
defaultContext = ScriptContext
  { scriptContextTxInfo = TxInfo
      { txInfoInputs = []
      , txInfoOutputs = []
      , txInfoFee = Ada.lovelaceValueOf 0
      , txInfoMint = Ada.lovelaceValueOf 0
      , txInfoDCert = []
      , txInfoWdrl = []
      , txInfoValidRange = always
      , txInfoSignatories = []
      , txInfoData = []
      , txInfoId = "test-tx-id"
      }
  , scriptContextPurpose = Minting "test-policy-id"
  }

instance Arbitrary CSG where
  arbitrary = CSG
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> choose (1, 52)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> choose (2, 100)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash . toBuiltin <$> arbitrary @ByteString

instance Arbitrary ValidatorHash where
  arbitrary = ValidatorHash . toBuiltin <$> arbitrary @ByteString

instance Arbitrary POSIXTime where
  arbitrary = POSIXTime <$> arbitrary
