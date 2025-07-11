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
import Servant.Server (err404, err400, err500)
import Control.Monad.Except (throwError)
import CSG (CSG(..), CreateCSGRequest(..), JoinCSGRequest(..), ClaimRewardRequest(..), WithdrawRequest(..))
import qualified CSG
import qualified Cardano
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.Char8 as L8
import Database.PostgreSQL.Simple (Connection, FromRow(..), ToRow(..), Only(..), Query)
import qualified Database.PostgreSQL.Simple as PG
import Control.Exception (try, SomeException)
import Data.Proxy (Proxy(..))
import Servant.API
import qualified Data.Text as T
import Data.Hashable (hash)
import Numeric (showHex)
import Control.Monad (when)

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
    createCSG CreateCSGRequest{..} = do
      -- Input validation
      when (T.null createCsgName || T.length createCsgName < 3) $
        throwError err400 { errBody = L8.pack "CSG name must be at least 3 characters" }
      when (createCsgStakeAmount <= 0) $
        throwError err400 { errBody = L8.pack "Stake amount must be positive" }
      when (createCsgDuration <= 0 || createCsgDuration > 365) $
        throwError err400 { errBody = L8.pack "Duration must be between 1 and 365 days" }
      
      currentTime <- liftIO getCurrentTime
      let endTime = addUTCTime (fromIntegral $ createCsgDuration * 86400) currentTime
      
      -- Generate unique CSG ID from hash of name + timestamp  
      let csgData = T.unpack createCsgName ++ show currentTime
      let csgHash = abs (hash csgData)
      let csgId = T.pack $ "csg_" ++ showHex csgHash ""
      
      -- Generate contract address for this CSG
      let contractAddress = Cardano.generateCSGAddress csgId
      
      -- Store CSG in database with contract address
      _ <- liftIO $ PG.execute conn 
        "INSERT INTO csgs (id, name, contract_address, stake_amount, duration, start_time, end_time, status) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (csgId, createCsgName, contractAddress, createCsgStakeAmount, createCsgDuration, currentTime, endTime, "Active" :: T.Text)
      
      return $ CSG csgId createCsgName [] createCsgStakeAmount createCsgDuration currentTime endTime "Active"

    joinCSG :: T.Text -> JoinCSGRequest -> Handler CSG
    joinCSG csgId JoinCSGRequest{..} = do
      -- Input validation
      when (T.null csgId || T.length csgId < 5) $
        throwError err400 { errBody = L8.pack "Invalid CSG ID" }
      when (joinStakeAmount <= 0) $
        throwError err400 { errBody = L8.pack "Stake amount must be positive" }
      
      csgRows <- liftIO $ PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> throwError err404 { errBody = L8.pack "CSG not found" }
        (csg:_) -> do
          txResult <- liftIO $ Cardano.createJoinTransaction csgId joinStakeAmount
          case txResult of
            Right txHash -> do
              _ <- liftIO $ PG.execute conn 
                "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type, amount) VALUES (?, ?, ?, ?)"
                (csgId, txHash, "JOIN" :: T.Text, joinStakeAmount)
              return csg
            Left err -> throwError err500 { errBody = L8.pack ("Transaction failed: " ++ err) }

    claimReward :: T.Text -> ClaimRewardRequest -> Handler CSG
    claimReward csgId ClaimRewardRequest{..} = do
      csgRows <- liftIO $ PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> throwError err404 { errBody = L8.pack "CSG not found" }
        (csg:_) -> do
          txResult <- liftIO $ Cardano.createClaimTransaction csgId claimantAddress
          case txResult of
            Right txHash -> do
              _ <- liftIO $ PG.execute conn 
                "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type, participant_address) VALUES (?, ?, ?, ?)"
                (csgId, txHash, "CLAIM" :: T.Text, claimantAddress)
              return csg
            Left err -> throwError err500 { errBody = L8.pack ("Transaction failed: " ++ err) }

    closeCSG :: T.Text -> Handler CSG
    closeCSG csgId = do
      csgRows <- liftIO $ PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> throwError err404 { errBody = L8.pack "CSG not found" }
        (csg:_) -> do
          txResult <- liftIO $ Cardano.createCloseTransaction csgId
          case txResult of
            Right txHash -> do
              _ <- liftIO $ PG.execute conn "UPDATE csgs SET status = ? WHERE id = ?" ("Closed" :: T.Text, csgId)
              _ <- liftIO $ PG.execute conn 
                "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type) VALUES (?, ?, ?)"
                (csgId, txHash, "CLOSE" :: T.Text)
              return $ csg { csgStatus = "Closed" }
            Left err -> throwError err500 { errBody = L8.pack ("Transaction failed: " ++ err) }

    listCSGs :: Handler [CSG]
    listCSGs = liftIO $ do
      rows <- PG.query_ conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs ORDER BY created_at DESC"
      return $ map (\(csgId, name, stake, duration, start, end, status) -> 
        CSG csgId name [] stake duration start end status) rows

    withdraw :: T.Text -> WithdrawRequest -> Handler CSG
    withdraw csgId WithdrawRequest{..} = do
      csgRows <- liftIO $ PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> throwError err404 { errBody = L8.pack "CSG not found" }
        (csg:_) -> do
          txResult <- liftIO $ Cardano.createWithdrawTransaction csgId withdrawAddress withdrawAmount
          case txResult of
            Right txHash -> do
              _ <- liftIO $ PG.execute conn 
                "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type, participant_address, amount) VALUES (?, ?, ?, ?, ?)"
                (csgId, txHash, "WITHDRAW" :: T.Text, withdrawAddress, withdrawAmount)
              return csg
            Left err -> throwError err500 { errBody = L8.pack ("Transaction failed: " ++ err) }

app :: Connection -> Application
app conn = serve api (server conn)

-- Add any additional helper functions or type definitions here
