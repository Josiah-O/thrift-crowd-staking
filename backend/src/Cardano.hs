{-# LANGUAGE OverloadedStrings #-}

module Cardano where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Data.Hashable (hash)

-- Blockfrost configuration with validation
getBlockfrostBaseUrl :: IO String
getBlockfrostBaseUrl = do
  maybeUrl <- lookupEnv "BLOCKFROST_BASE_URL"
  return $ fromMaybe "https://cardano-preprod.blockfrost.io/api/v0" maybeUrl

getBlockfrostProjectId :: IO (Either String Text)
getBlockfrostProjectId = do
  maybeId <- lookupEnv "BLOCKFROST_PROJECT_ID"
  case maybeId of
    Nothing -> return $ Left "BLOCKFROST_PROJECT_ID environment variable not set"
    Just "" -> return $ Left "BLOCKFROST_PROJECT_ID environment variable is empty"
    Just projectId 
      | "preprod" `isInfixOf` projectId -> return $ Right (T.pack projectId)
      | "testnet" `isInfixOf` projectId -> return $ Right (T.pack projectId)
      | otherwise -> return $ Left "BLOCKFROST_PROJECT_ID must contain 'preprod' or 'testnet'"
  where
    isInfixOf needle haystack = needle `elem` words haystack

-- Submit transaction to Cardano testnet
submitTransaction :: Text -> IO (Either String Text)
submitTransaction txCbor = do
  projectIdResult <- getBlockfrostProjectId
  case projectIdResult of
    Left err -> return $ Left err
    Right projectId -> do
      baseUrl <- getBlockfrostBaseUrl
      manager <- newManager tlsManagerSettings
      let url = baseUrl ++ "/tx/submit"
      
      initReq <- parseRequest url
      let req = initReq
            { method = "POST"
            , requestHeaders = 
              [ ("project_id", BS.pack $ T.unpack projectId)
              , ("Content-Type", "application/cbor")
              ]
            , requestBody = RequestBodyBS (BS.pack $ T.unpack txCbor)
            }
      
      response <- httpLbs req manager
      let respBody = responseBody response
      let statusCode' = statusCode $ responseStatus response
      
      if statusCode' == 200
        then case decode respBody of
          Just (Object obj) -> case parseMaybe (.: "hash") obj of
            Just hash -> return $ Right hash
            Nothing -> return $ Left "No hash in response"
          _ -> return $ Left "Invalid JSON response"
        else return $ Left $ "HTTP error: " ++ show statusCode'

-- Query UTXOs for an address
queryAddressUtxos :: Text -> IO [Value]
queryAddressUtxos address = do
  projectIdResult <- getBlockfrostProjectId
  case projectIdResult of
    Left _ -> return []  -- Return empty list if no valid project ID
    Right projectId -> do
      baseUrl <- getBlockfrostBaseUrl
      manager <- newManager tlsManagerSettings
      let url = baseUrl ++ "/addresses/" ++ T.unpack address ++ "/utxos"
      
      initReq <- parseRequest url
      let req = initReq
            { requestHeaders = [("project_id", BS.pack $ T.unpack projectId)] }
      
      response <- httpLbs req manager
      let respBody = responseBody response
      
      case decode respBody of
        Just utxos -> return utxos
        Nothing -> return []

-- Generate a contract address for CSG
generateCSGAddress :: Text -> Text
generateCSGAddress csgId = 
  "addr_test1qz" <> T.take 56 (csgId <> csgId <> csgId <> csgId <> csgId) <> "7jhxvfk"

-- Create transaction for joining a CSG (improved with validation)
createJoinTransaction :: Text -> Integer -> IO (Either String Text)
createJoinTransaction csgId amount
  | amount <= 0 = return $ Left "Amount must be positive"
  | T.length csgId < 5 = return $ Left "Invalid CSG ID"
  | otherwise = do
      -- For now, create a more realistic-looking transaction hash
      -- In production, this would build a proper Cardano transaction
      let txData = T.unpack csgId ++ show amount ++ "JOIN"
      let hashValue = abs $ hash txData
      let mockTxHash = "tx_" ++ take 64 (cycle $ show hashValue)
      return $ Right (T.pack mockTxHash)

-- Create transaction for claiming rewards (improved with validation)
createClaimTransaction :: Text -> Text -> IO (Either String Text)
createClaimTransaction csgId address
  | T.length csgId < 5 = return $ Left "Invalid CSG ID"
  | T.length address < 10 = return $ Left "Invalid Cardano address"
  | otherwise = do
      let txData = T.unpack csgId ++ T.unpack address ++ "CLAIM"
      let hashValue = abs $ hash txData
      let mockTxHash = "tx_" ++ take 64 (cycle $ show hashValue)
      return $ Right (T.pack mockTxHash)

-- Create transaction for closing CSG (improved with validation)
createCloseTransaction :: Text -> IO (Either String Text)
createCloseTransaction csgId
  | T.length csgId < 5 = return $ Left "Invalid CSG ID"
  | otherwise = do
      let txData = T.unpack csgId ++ "CLOSE"
      let hashValue = abs $ hash txData
      let mockTxHash = "tx_" ++ take 64 (cycle $ show hashValue)
      return $ Right (T.pack mockTxHash)

-- Create transaction for withdrawing from CSG (improved with validation)
createWithdrawTransaction :: Text -> Text -> Integer -> IO (Either String Text)
createWithdrawTransaction csgId address amount
  | amount <= 0 = return $ Left "Amount must be positive"
  | T.length csgId < 5 = return $ Left "Invalid CSG ID"
  | T.length address < 10 = return $ Left "Invalid Cardano address"
  | otherwise = do
      let txData = T.unpack csgId ++ T.unpack address ++ show amount ++ "WITHDRAW"
      let hashValue = abs $ hash txData
      let mockTxHash = "tx_" ++ take 64 (cycle $ show hashValue)
      return $ Right (T.pack mockTxHash) 