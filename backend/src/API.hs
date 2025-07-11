{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API 
  ( app
  , API
  , server
  -- export other necessary functions and types here
  ) where

import Servant
import CSG (CSG(..), CreateCSGRequest(..), JoinCSGRequest(..), ClaimRewardRequest(..), WithdrawRequest(..))
import qualified CSG
import qualified Cardano
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection, FromRow(..), ToRow(..), field, Only(..), Query)
import qualified Database.PostgreSQL.Simple as PG
import Control.Exception (try, SomeException)
import Data.Proxy (Proxy(..))
import Servant.API
import qualified Data.Text as T
import Data.Hashable (hash)
import Numeric (showHex)

type API = 
  "api" :> 
    (    "health" :> Get '[JSON] NoContent
    :<|> "create-csg" :> ReqBody '[JSON] CreateCSGRequest :> Post '[JSON] CSG
    :<|> "join-csg" :> Capture "csgId" T.Text :> ReqBody '[JSON] JoinCSGRequest :> Post '[JSON] CSG
    :<|> "claim-reward" :> Capture "csgId" T.Text :> ReqBody '[JSON] ClaimRewardRequest :> Post '[JSON] CSG
    :<|> "close-csg" :> Capture "csgId" T.Text :> Post '[JSON] CSG
    :<|> "list-csgs" :> Get '[JSON] [CSG]
    :<|> "withdraw" :> Capture "csgId" T.Text :> ReqBody '[JSON] WithdrawRequest :> Post '[JSON] CSG
    )

api :: Proxy API
api = Proxy

-- Database instances
instance FromRow CSG where
  fromRow = CSG <$> field <*> field <*> pure [] <*> field <*> field <*> field <*> field <*> field

server :: Connection -> Server API
server conn = healthCheck
         :<|> createCSG
         :<|> joinCSG
         :<|> claimReward
         :<|> closeCSG
         :<|> listCSGs
         :<|> withdraw
  where
    healthCheck :: Handler NoContent
    healthCheck = return NoContent
    
    createCSG :: CreateCSGRequest -> Handler CSG
    createCSG CreateCSGRequest{..} = liftIO $ do
      currentTime <- getCurrentTime
      let endTime = addUTCTime (fromIntegral $ createCsgDuration * 86400) currentTime
      
      -- Generate unique CSG ID from hash of name + timestamp  
      let csgData = T.unpack createCsgName ++ show currentTime
      let csgHash = abs (hash csgData)
      let csgId = T.pack $ "csg_" ++ showHex csgHash ""
      
      -- Generate contract address for this CSG
      let contractAddress = Cardano.generateCSGAddress csgId
      
      -- Store CSG in database with contract address
      _ <- PG.execute conn 
        "INSERT INTO csgs (id, name, contract_address, stake_amount, duration, start_time, end_time, status) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (csgId, createCsgName, contractAddress, createCsgStakeAmount, createCsgDuration, currentTime, endTime, "Active" :: T.Text)
      
      return $ CSG csgId createCsgName [] createCsgStakeAmount createCsgDuration currentTime endTime "Active"

    joinCSG :: T.Text -> JoinCSGRequest -> Handler CSG
    joinCSG csgId JoinCSGRequest{..} = liftIO $ do
      csgRows <- PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> error "CSG not found"
        (csg:_) -> do
          txResult <- Cardano.createJoinTransaction csgId joinStakeAmount
          case txResult of
            Right txHash -> do
              _ <- PG.execute conn 
                "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type, amount) VALUES (?, ?, ?, ?)"
                (csgId, txHash, "JOIN" :: T.Text, joinStakeAmount)
              return csg
            Left err -> error err

    claimReward :: T.Text -> ClaimRewardRequest -> Handler CSG
    claimReward csgId ClaimRewardRequest{..} = liftIO $ do
      csgRows <- PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> error "CSG not found"
        (csg:_) -> do
          txResult <- Cardano.createClaimTransaction csgId claimantAddress
          case txResult of
            Right txHash -> do
              _ <- PG.execute conn 
                "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type, participant_address) VALUES (?, ?, ?, ?)"
                (csgId, txHash, "CLAIM" :: T.Text, claimantAddress)
              return csg
            Left err -> error err

    closeCSG :: T.Text -> Handler CSG
    closeCSG csgId = liftIO $ do
      csgRows <- PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> error "CSG not found"
        (csg:_) -> do
          txResult <- Cardano.createCloseTransaction csgId
          case txResult of
            Right txHash -> do
              _ <- PG.execute conn "UPDATE csgs SET status = ? WHERE id = ?" ("Closed" :: T.Text, csgId)
              _ <- PG.execute conn 
                "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type) VALUES (?, ?, ?)"
                (csgId, txHash, "CLOSE" :: T.Text)
              return $ csg { csgStatus = "Closed" }
            Left err -> error err

    listCSGs :: Handler [CSG]
    listCSGs = liftIO $ do
      rows <- PG.query_ conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs ORDER BY created_at DESC"
      return $ map (\(csgId, name, stake, duration, start, end, status) -> 
        CSG csgId name [] stake duration start end status) rows

    withdraw :: T.Text -> WithdrawRequest -> Handler CSG
    withdraw csgId WithdrawRequest{..} = liftIO $ do
      csgRows <- PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> error "CSG not found"
        (csg:_) -> do
          txResult <- Cardano.createWithdrawTransaction csgId withdrawAddress withdrawAmount
          case txResult of
            Right txHash -> do
              _ <- PG.execute conn 
                "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type, participant_address, amount) VALUES (?, ?, ?, ?, ?)"
                (csgId, txHash, "WITHDRAW" :: T.Text, withdrawAddress, withdrawAmount)
              return csg
            Left err -> error err

app :: Connection -> Application
app conn = serve api (server conn)

-- Add any additional helper functions or type definitions here
