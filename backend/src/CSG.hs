{-# LANGUAGE DeriveGeneric #-}

module CSG (
    CSG(..),
    CreateCSGRequest(..),
    JoinCSGRequest(..),
    ClaimRewardRequest(..),
    WithdrawRequest(..),
    -- other exports...
) where

import GHC.Generics
import Data.Aeson

data CSG = CSG
    { csgId :: Int
    , csgName :: String
    , csgGoal :: Int
    , csgDuration :: Int
    , csgParticipants :: [String]
    , csgTotalContribution :: Int
    } deriving (Show, Generic)

data CreateCSGRequest = CreateCSGRequest
    { createName :: String
    , createGoal :: Int
    , createDuration :: Int
    } deriving (Show, Generic)

data JoinCSGRequest = JoinCSGRequest
    { joinAmount :: Int
    } deriving (Show, Generic)

data ClaimRewardRequest = ClaimRewardRequest
    { claimantId :: String
    } deriving (Show, Generic)

data WithdrawRequest = WithdrawRequest
    { withdrawAmount :: Int
    } deriving (Show, Generic)

instance ToJSON CSG
instance FromJSON CSG
instance ToJSON CreateCSGRequest
instance FromJSON CreateCSGRequest
instance ToJSON JoinCSGRequest
instance FromJSON JoinCSGRequest
instance ToJSON ClaimRewardRequest
instance FromJSON ClaimRewardRequest
instance ToJSON WithdrawRequest
instance FromJSON WithdrawRequest

-- Add any additional functions or implementations here
