{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CSG where

import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)

data CSG = CSG
  { csgId :: Text
  , csgName :: Text
  , csgParticipants :: [Text]
  , csgTotalStake :: Integer
  , csgDuration :: Integer
  , csgStartTime :: UTCTime
  , csgEndTime :: UTCTime
  , csgStatus :: Text
  } deriving (Show, Generic)

instance ToJSON CSG
instance FromJSON CSG

-- Database instance - matches SQL query: "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs"
instance FromRow CSG where
  fromRow = CSG <$> field          -- id -> csgId
                <*> field          -- name -> csgName
                <*> pure []        -- participants (not selected, use empty list)
                <*> field          -- stake_amount -> csgTotalStake
                <*> field          -- duration -> csgDuration
                <*> field          -- start_time -> csgStartTime
                <*> field          -- end_time -> csgEndTime
                <*> field          -- status -> csgStatus

data CreateCSGRequest = CreateCSGRequest
  { createCsgName :: Text
  , createCsgDuration :: Integer
  , createCsgStakeAmount :: Integer
  } deriving (Show, Generic)

instance ToJSON CreateCSGRequest
instance FromJSON CreateCSGRequest

data JoinCSGRequest = JoinCSGRequest
  { joinStakeAmount :: Integer
  } deriving (Show, Generic)

instance ToJSON JoinCSGRequest
instance FromJSON JoinCSGRequest

data ClaimRewardRequest = ClaimRewardRequest
  { claimantAddress :: Text
  } deriving (Show, Generic)

instance ToJSON ClaimRewardRequest
instance FromJSON ClaimRewardRequest

data WithdrawRequest = WithdrawRequest
  { withdrawAmount :: Integer
  , withdrawAddress :: Text
  } deriving (Show, Generic)

instance ToJSON WithdrawRequest
instance FromJSON WithdrawRequest

-- Add any additional functions or implementations here
