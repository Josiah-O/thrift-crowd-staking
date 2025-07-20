{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Database.PostgreSQL.Simple
import Servant
import API (app)
import CSG (CSG(..), CreateCSGRequest(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Web.HttpApiData (ToHttpApiData(..))
import System.Environment (lookupEnv)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Control.Monad (when)

main :: IO ()
main = do
  putStrLn "Starting Thrift Crowd Staking DApp backend on port 8080..."
  
  -- Get database URL from environment - fixed to match docker-compose.yml config
  dbUrl <- lookupEnv "DATABASE_URL"
  let connStr = BS.pack $ fromMaybe "postgresql://postgres:password@db:5432/thrift_crowd_staking" dbUrl
  
  putStrLn $ "Attempting to connect to database: " ++ show connStr
  
  -- Wait for database to be ready
  conn <- waitForDatabase connStr 30  -- Wait up to 30 seconds
  
  -- Initialize database schema
  initializeDatabase conn
  
  let corsPolicy = simpleCorsResourcePolicy 
        { corsOrigins = Just (["http://localhost:3000"], True)
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        , corsRequestHeaders = ["Content-Type"]
        }
  
  putStrLn "Available endpoints (DApp - no authentication required):"
  putStrLn "  GET /api/health - Health check"
  putStrLn "  POST /api/create-csg - Create new CSG"
  putStrLn "  POST /api/join-csg/{id} - Join CSG"
  putStrLn "  POST /api/claim-reward/{id} - Claim rewards"
  putStrLn "  POST /api/close-csg/{id} - Close CSG"
  putStrLn "  GET /api/list-csgs - List active CSGs"
  putStrLn "  POST /api/withdraw/{id} - Withdraw from CSG"
  putStrLn ""
  putStrLn "Pure DApp: All operations are wallet-based, no backend authentication!"
  
  run 8080 $ cors (const $ Just corsPolicy) $ app conn

-- Wait for database to be available with retry logic
waitForDatabase :: BS.ByteString -> Int -> IO Connection
waitForDatabase connStr maxRetries = go maxRetries
  where
    go 0 = error "Failed to connect to database after maximum retries"
    go n = do
      putStrLn $ "Attempting database connection (attempt " ++ show (maxRetries - n + 1) ++ "/" ++ show maxRetries ++ ")..."
      result <- try (connectPostgreSQL connStr)
      case result of
        Right conn -> do
          putStrLn "Successfully connected to database!"
          return conn
        Left (e :: SomeException) -> do
          putStrLn $ "Database connection failed: " ++ show e
          when (n > 1) $ do
            putStrLn "Retrying in 2 seconds..."
            threadDelay 2000000  -- Wait 2 seconds
          go (n - 1)

-- Initialize database tables
initializeDatabase :: Connection -> IO ()
initializeDatabase conn = do
  putStrLn "Initializing database..."
  
  -- Create users table first
  -- Auth.createUserTable conn -- Removed auth-related initialization
  
  _ <- execute_ conn $ fromString $ unlines
    [ "CREATE TABLE IF NOT EXISTS csgs ("
    , "    id VARCHAR(64) PRIMARY KEY,"
    , "    name VARCHAR(255) NOT NULL,"
    , "    contract_address VARCHAR(255) NOT NULL,"
    , "    stake_amount BIGINT NOT NULL,"
    , "    duration INTEGER NOT NULL,"
    , "    start_time TIMESTAMP NOT NULL,"
    , "    end_time TIMESTAMP NOT NULL,"
    , "    status VARCHAR(50) NOT NULL,"
    , "    owner_address VARCHAR(255),"  -- Add owner_address field
    , "    pool_id VARCHAR(64),"
    , "    delegation_tx_hash VARCHAR(128),"
    , "    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    , ");"
    , "CREATE TABLE IF NOT EXISTS csg_participants ("
    , "    id SERIAL PRIMARY KEY,"
    , "    csg_id VARCHAR(64) REFERENCES csgs(id),"
    , "    participant_address VARCHAR(255) NOT NULL,"
    , "    stake_amount BIGINT NOT NULL,"
    , "    join_tx_hash VARCHAR(128) NOT NULL,"
    , "    joined_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    , ");"
    , "CREATE TABLE IF NOT EXISTS csg_transactions ("
    , "    id SERIAL PRIMARY KEY,"
    , "    csg_id VARCHAR(64) REFERENCES csgs(id),"
    , "    tx_hash VARCHAR(128) NOT NULL,"
    , "    tx_type VARCHAR(50) NOT NULL,"
    , "    participant_address VARCHAR(255),"
    , "    amount BIGINT,"
    , "    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    , ");"
    , "CREATE TABLE IF NOT EXISTS csg_rewards ("
    , "    id SERIAL PRIMARY KEY,"
    , "    csg_id VARCHAR(64) REFERENCES csgs(id),"
    , "    participant_address VARCHAR(255) NOT NULL,"
    , "    reward_amount BIGINT NOT NULL,"
    , "    reward_epoch INTEGER NOT NULL,"
    , "    claimed BOOLEAN DEFAULT FALSE,"
    , "    claim_tx_hash VARCHAR(128),"
    , "    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
    , ");"
    , "CREATE INDEX IF NOT EXISTS idx_csgs_status ON csgs(status);"
    , "CREATE INDEX IF NOT EXISTS idx_csgs_pool_id ON csgs(pool_id);"
    , "CREATE INDEX IF NOT EXISTS idx_csgs_owner ON csgs(owner_address);"  -- Add owner index
    , "CREATE INDEX IF NOT EXISTS idx_csg_participants_csg_id ON csg_participants(csg_id);"
    , "CREATE INDEX IF NOT EXISTS idx_csg_participants_address ON csg_participants(participant_address);"
    , "CREATE INDEX IF NOT EXISTS idx_csg_transactions_csg_id ON csg_transactions(csg_id);"
    , "CREATE INDEX IF NOT EXISTS idx_csg_transactions_tx_hash ON csg_transactions(tx_hash);"
    , "CREATE INDEX IF NOT EXISTS idx_csg_rewards_csg_id ON csg_rewards(csg_id);"
    , "CREATE INDEX IF NOT EXISTS idx_csg_rewards_participant ON csg_rewards(participant_address);"
    , "CREATE INDEX IF NOT EXISTS idx_csg_rewards_claimed ON csg_rewards(claimed);"
    ]
  
  -- Add owner_address column to existing CSGs table if it doesn't exist
  _ <- execute_ conn "ALTER TABLE csgs ADD COLUMN IF NOT EXISTS owner_address VARCHAR(255);"
  
  putStrLn "Database initialized successfully"
