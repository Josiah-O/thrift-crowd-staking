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
import Servant.Auth.Server
import Servant.Server (err404, err400, err500)
import Control.Monad.Except (throwError)
import CSG (CSG(..), CreateCSGRequest(..), JoinCSGRequest(..), ClaimRewardRequest(..), WithdrawRequest(..))
import qualified CSG
import qualified Cardano
import qualified Auth
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
    :<|> "auth" :> "wallet" :> ReqBody '[JSON] Auth.WalletAuthRequest :> Post '[JSON] Auth.LoginResponse
    :<|> Auth (Auth '[JWT] Auth.AuthenticatedUser) :> "create-csg" :> ReqBody '[JSON] CreateCSGRequest :> Post '[JSON] CSG
    :<|> Auth (Auth '[JWT] Auth.AuthenticatedUser) :> "join-csg" :> Capture "csgId" T.Text :> ReqBody '[JSON] JoinCSGRequest :> Post '[JSON] CSG
    :<|> Auth (Auth '[JWT] Auth.AuthenticatedUser) :> "claim-reward" :> Capture "csgId" T.Text :> ReqBody '[JSON] ClaimRewardRequest :> Post '[JSON] CSG
    :<|> Auth (Auth '[JWT] Auth.AuthenticatedUser) :> "close-csg" :> Capture "csgId" T.Text :> Post '[JSON] CSG
    :<|> Auth (Auth '[JWT] Auth.AuthenticatedUser) :> "list-csgs" :> Get '[JSON] [CSG]
    :<|> Auth (Auth '[JWT] Auth.AuthenticatedUser) :> "withdraw" :> Capture "csgId" T.Text :> ReqBody '[JSON] WithdrawRequest :> Post '[JSON] CSG
    )

api :: Proxy API
api = Proxy

server :: Connection -> JWTSettings -> Server API
server conn jwtCfg = healthCheck
         :<|> walletAuthEndpoint
         :<|> createCSG
         :<|> joinCSG
         :<|> claimReward
         :<|> closeCSG
         :<|> listCSGs
         :<|> withdraw
  where
    healthCheck :: Handler NoContent
    healthCheck = return NoContent
    
    walletAuthEndpoint :: Auth.WalletAuthRequest -> Handler Auth.LoginResponse
    walletAuthEndpoint walletReq = do
      result <- liftIO $ Auth.authenticateWallet conn walletReq
      case result of
        Left err -> throwError err400 { errBody = L8.pack err }
        Right user -> do
          maybeToken <- liftIO $ Auth.generateJWT jwtCfg user
          case maybeToken of
            Nothing -> throwError err500 { errBody = L8.pack "Failed to generate JWT token" }
            Just token -> return $ Auth.LoginResponse token user
    
    createCSG :: Auth.AuthResult -> CreateCSGRequest -> Handler CSG
    createCSG authResult CreateCSGRequest{..} = do
      user <- Auth.requireAuth authResult
      
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
      
      -- Store CSG in database with authenticated user as owner
      _ <- liftIO $ PG.execute conn 
        "INSERT INTO csgs (id, name, contract_address, stake_amount, duration, start_time, end_time, status, owner_address) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        (csgId, createCsgName, contractAddress, createCsgStakeAmount, createCsgDuration, currentTime, endTime, "Active" :: T.Text, Auth.getUserAddress user)
      
      return $ CSG csgId createCsgName [] createCsgStakeAmount createCsgDuration currentTime endTime "Active"

    joinCSG :: Auth.AuthResult -> T.Text -> JoinCSGRequest -> Handler CSG
    joinCSG authResult csgId JoinCSGRequest{..} = do
      user <- Auth.requireAuth authResult
      let userAddress = Auth.getUserAddress user
      
      -- Input validation
      when (T.null csgId || T.length csgId < 5) $
        throwError err400 { errBody = L8.pack "Invalid CSG ID" }
      when (joinStakeAmount <= 0) $
        throwError err400 { errBody = L8.pack "Stake amount must be positive" }
      
      csgRows <- liftIO $ PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> throwError err404 { errBody = L8.pack "CSG not found" }
        (csg:_) -> do
          -- Check if user already joined this CSG
          existingParticipants <- liftIO $ PG.query conn 
            "SELECT COUNT(*) FROM csg_participants WHERE csg_id = ? AND participant_address = ?"
            (csgId, userAddress)
          
          case existingParticipants of
            [(count :: Int)] | count > 0 -> 
              throwError err400 { errBody = L8.pack "User already joined this CSG" }
            _ -> do
              -- Create join transaction with automatic delegation
              txResult <- liftIO $ Cardano.createJoinTransaction csgId joinStakeAmount
              case txResult of
                Right txHash -> do
                  -- Record the join transaction
                  _ <- liftIO $ PG.execute conn 
                    "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type, amount) VALUES (?, ?, ?, ?)"
                    (csgId, txHash, "JOIN" :: T.Text, joinStakeAmount)
                  
                  -- Add participant to CSG with real user address
                  _ <- liftIO $ PG.execute conn 
                    "INSERT INTO csg_participants (csg_id, participant_address, stake_amount, join_tx_hash) VALUES (?, ?, ?, ?)"
                    (csgId, userAddress, joinStakeAmount, txHash)
                  
                  -- Update total stake in CSG
                  _ <- liftIO $ PG.execute conn 
                    "UPDATE csgs SET stake_amount = stake_amount + ? WHERE id = ?"
                    (joinStakeAmount, csgId)
                  
                  -- Check if this is the first participant - if so, delegate to pool
                  participantCount <- liftIO $ PG.query conn 
                    "SELECT COUNT(*) FROM csg_participants WHERE csg_id = ?"
                    (Only csgId)
                  
                  case participantCount of
                    [(count :: Int)] | count == 1 -> do
                      -- First participant - initiate staking delegation
                      totalStake <- liftIO $ PG.query conn 
                        "SELECT stake_amount FROM csgs WHERE id = ?"
                        (Only csgId)
                      
                      case totalStake of
                        [(amount :: Integer)] -> do
                          delegationResult <- liftIO $ Cardano.delegateCSGToPoolWithDiversification conn csgId amount
                          case delegationResult of
                            Right delegationTxHash -> do
                              -- Record delegation transaction
                              _ <- liftIO $ PG.execute conn 
                                "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type) VALUES (?, ?, ?)"
                                (csgId, delegationTxHash, "DELEGATE" :: T.Text)
                              
                              -- Update CSG with delegation info
                              _ <- liftIO $ PG.execute conn 
                                "UPDATE csgs SET delegation_tx_hash = ? WHERE id = ?"
                                (delegationTxHash, csgId)
                              
                              return csg
                            Left err -> 
                              throwError err500 { errBody = L8.pack ("Delegation failed: " ++ err) }
                        _ -> throwError err500 { errBody = L8.pack "Failed to get CSG stake amount" }
                    _ -> return csg
                    
                Left err -> throwError err500 { errBody = L8.pack ("Join transaction failed: " ++ err) }

    claimReward :: Auth.AuthResult -> T.Text -> ClaimRewardRequest -> Handler CSG
    claimReward authResult csgId ClaimRewardRequest{..} = do
      user <- Auth.requireAuth authResult
      let userAddress = Auth.getUserAddress user
      
      -- Input validation
      when (T.null csgId || T.length csgId < 5) $
        throwError err400 { errBody = L8.pack "Invalid CSG ID" }
      
      csgRows <- liftIO $ PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> throwError err404 { errBody = L8.pack "CSG not found" }
        (csg:_) -> do
          -- Check if CSG duration has ended
          currentTime <- liftIO getCurrentTime
          when (currentTime < csgEndTime csg) $
            throwError err400 { errBody = L8.pack "CSG duration has not ended yet" }
          
          -- Verify user is a participant
          participantRows <- liftIO $ PG.query conn 
            "SELECT stake_amount FROM csg_participants WHERE csg_id = ? AND participant_address = ?"
            (csgId, userAddress)
          
          case participantRows of
            [] -> throwError err404 { errBody = L8.pack "User is not a participant in this CSG" }
            [(userStake :: Integer)] -> do
              -- Get all participants for reward calculation
              participantList <- liftIO $ PG.query conn 
                "SELECT participant_address, stake_amount FROM csg_participants WHERE csg_id = ?"
                (Only csgId)
              
              -- Calculate actual staking rewards
              let csgAddr = Cardano.generateCSGAddress csgId
              currentTime <- liftIO getCurrentTime
              let startTime = addUTCTime (-2592000) currentTime  -- 30 days ago
              
              rewardsResult <- liftIO $ Cardano.calculateCSGRewards csgAddr startTime currentTime
              case rewardsResult of
                Left err -> throwError err500 { errBody = L8.pack ("Failed to calculate rewards: " ++ err) }
                Right totalRewards -> do
                  -- Calculate proportional rewards for all participants
                  let rewardDistribution = Cardano.calculateProportionalRewards participantList totalRewards
                  
                  -- Find this user's reward
                  case lookup userAddress rewardDistribution of
                    Nothing -> throwError err404 { errBody = L8.pack "User not found in reward distribution" }
                    Just userReward -> do
                      -- Create claim transaction
                      txResult <- liftIO $ Cardano.createClaimTransaction csgId userAddress
                      case txResult of
                        Right txHash -> do
                          -- Record the claim transaction
                          _ <- liftIO $ PG.execute conn 
                            "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type, participant_address, amount) VALUES (?, ?, ?, ?, ?)"
                            (csgId, txHash, "CLAIM" :: T.Text, userAddress, userReward)
                          
                          -- Record the reward as claimed
                          _ <- liftIO $ PG.execute conn 
                            "INSERT INTO csg_rewards (csg_id, participant_address, reward_amount, reward_epoch, claimed, claim_tx_hash) VALUES (?, ?, ?, ?, ?, ?)"
                            (csgId, userAddress, userReward, 0 :: Integer, True, txHash)
                          
                          return csg
                        Left err -> throwError err500 { errBody = L8.pack ("Claim transaction failed: " ++ err) }

    closeCSG :: Auth.AuthResult -> T.Text -> Handler CSG
    closeCSG authResult csgId = do
      user <- Auth.requireAuth authResult
      let userAddress = Auth.getUserAddress user
      
      -- Input validation
      when (T.null csgId || T.length csgId < 5) $
        throwError err400 { errBody = L8.pack "Invalid CSG ID" }
      
      csgRows <- liftIO $ PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status, owner_address FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> throwError err404 { errBody = L8.pack "CSG not found" }
        (csg:_) -> do
          -- Verify user is the owner
          when (csgOwnerAddress csg /= userAddress) $
            throwError err403 { errBody = L8.pack "Only the CSG owner can close it" }
          
          txResult <- liftIO $ Cardano.createCloseTransaction csgId
          case txResult of
            Right txHash -> do
              -- Record closure transaction
              _ <- liftIO $ PG.execute conn 
                "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type) VALUES (?, ?, ?)"
                (csgId, txHash, "CLOSE" :: T.Text)
              
              -- Update CSG status
              _ <- liftIO $ PG.execute conn 
                "UPDATE csgs SET status = ? WHERE id = ?"
                ("Closed" :: T.Text, csgId)
              
              return csg
            Left err -> throwError err500 { errBody = L8.pack ("Close transaction failed: " ++ err) }

    listCSGs :: Auth.AuthResult -> Handler [CSG]
    listCSGs authResult = do
      user <- Auth.requireAuth authResult
      
      csgRows <- liftIO $ PG.query_ conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE status = 'Active'"
      return csgRows

    withdraw :: Auth.AuthResult -> T.Text -> WithdrawRequest -> Handler CSG
    withdraw authResult csgId WithdrawRequest{..} = do
      user <- Auth.requireAuth authResult
      let userAddress = Auth.getUserAddress user
      
      -- Input validation
      when (T.null csgId || T.length csgId < 5) $
        throwError err400 { errBody = L8.pack "Invalid CSG ID" }
      when (withdrawAmount <= 0) $
        throwError err400 { errBody = L8.pack "Withdraw amount must be positive" }
      
      csgRows <- liftIO $ PG.query conn "SELECT id, name, stake_amount, duration, start_time, end_time, status FROM csgs WHERE id = ?" (Only csgId)
      case csgRows of
        [] -> throwError err404 { errBody = L8.pack "CSG not found" }
        (csg:_) -> do
          -- Verify user is a participant
          participantRows <- liftIO $ PG.query conn 
            "SELECT stake_amount FROM csg_participants WHERE csg_id = ? AND participant_address = ?"
            (csgId, userAddress)
          
          case participantRows of
            [] -> throwError err404 { errBody = L8.pack "User is not a participant in this CSG" }
            [(userStake :: Integer)] -> do
              when (withdrawAmount > userStake) $
                throwError err400 { errBody = L8.pack "Cannot withdraw more than staked amount" }
              
              txResult <- liftIO $ Cardano.createWithdrawTransaction csgId userAddress withdrawAmount
              case txResult of
                Right txHash -> do
                  -- Record withdrawal transaction
                  _ <- liftIO $ PG.execute conn 
                    "INSERT INTO csg_transactions (csg_id, tx_hash, tx_type, participant_address, amount) VALUES (?, ?, ?, ?, ?)"
                    (csgId, txHash, "WITHDRAW" :: T.Text, userAddress, withdrawAmount)
                  
                  -- Update participant stake
                  _ <- liftIO $ PG.execute conn 
                    "UPDATE csg_participants SET stake_amount = stake_amount - ? WHERE csg_id = ? AND participant_address = ?"
                    (withdrawAmount, csgId, userAddress)
                  
                  -- Update total CSG stake
                  _ <- liftIO $ PG.execute conn 
                    "UPDATE csgs SET stake_amount = stake_amount - ? WHERE id = ?"
                    (withdrawAmount, csgId)
                  
                  return csg
                Left err -> throwError err500 { errBody = L8.pack ("Withdraw transaction failed: " ++ err) }

app :: Connection -> Context '[CookieSettings, JWTSettings] -> Application
app conn ctx = serveWithContext api ctx (server conn jwtCfg)
  where
    jwtCfg = case ctx of
      _ :. jwtSettings :. _ -> jwtSettings
