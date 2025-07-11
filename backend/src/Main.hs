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

main :: IO ()
main = do
  putStrLn "Starting Thrift Crowd Staking backend on port 8080..."
  
  -- Get database URL from environment
  dbUrl <- lookupEnv "DATABASE_URL"
  let connStr = BS.pack $ fromMaybe "host=db dbname=thrift_crowd_staking user=postgres password=password" dbUrl
  
  conn <- connectPostgreSQL connStr
  
  -- Initialize database schema
  initializeDatabase conn
  
  let corsPolicy = simpleCorsResourcePolicy 
        { corsOrigins = Just (["http://localhost:3000"], True)
        , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
        , corsRequestHeaders = ["Authorization", "Content-Type"]
        }
  run 8080 $ cors (const $ Just corsPolicy) $ app conn

-- Initialize database tables
initializeDatabase :: Connection -> IO ()
initializeDatabase conn = do
  putStrLn "Initializing database..."
  _ <- execute_ conn $ Query $ BS.pack $ unlines
    [ "CREATE TABLE IF NOT EXISTS csgs ("
    , "    id VARCHAR(64) PRIMARY KEY,"
    , "    name VARCHAR(255) NOT NULL,"
    , "    contract_address VARCHAR(255) NOT NULL,"
    , "    stake_amount BIGINT NOT NULL,"
    , "    duration INTEGER NOT NULL,"
    , "    start_time TIMESTAMP NOT NULL,"
    , "    end_time TIMESTAMP NOT NULL,"
    , "    status VARCHAR(50) NOT NULL,"
    , "    participants TEXT[] DEFAULT '{}',"
    , "    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
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
    ]
  putStrLn "Database initialized successfully"
