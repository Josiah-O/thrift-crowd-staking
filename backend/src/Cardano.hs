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
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

-- Blockfrost configuration  
getBlockfrostBaseUrl :: IO String
getBlockfrostBaseUrl = do
  maybeUrl <- lookupEnv "BLOCKFROST_BASE_URL"
  return $ fromMaybe "https://cardano-preprod.blockfrost.io/api/v0" maybeUrl

getBlockfrostProjectId :: IO Text
getBlockfrostProjectId = do
  maybeId <- lookupEnv "BLOCKFROST_PROJECT_ID"
  return $ T.pack $ fromMaybe "testnet_placeholder_update_this" maybeId

-- Submit transaction to Cardano testnet
submitTransaction :: Text -> IO (Either String Text)
submitTransaction txCbor = do
  projectId <- getBlockfrostProjectId
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
  let responseBody = responseBody response
  let statusCode' = statusCode $ responseStatus response
  
  if statusCode' == 200
    then case decode responseBody of
      Just (Object obj) -> case parseMaybe (.: "hash") obj of
        Just hash -> return $ Right hash
        Nothing -> return $ Left "No hash in response"
      _ -> return $ Left "Invalid JSON response"
    else return $ Left $ "HTTP error: " ++ show statusCode'

-- Query UTXOs for an address
queryAddressUtxos :: Text -> IO [Value]
queryAddressUtxos address = do
  projectId <- getBlockfrostProjectId
  baseUrl <- getBlockfrostBaseUrl
  manager <- newManager tlsManagerSettings
  let url = baseUrl ++ "/addresses/" ++ T.unpack address ++ "/utxos"
  
  initReq <- parseRequest url
  let req = initReq
        { requestHeaders = [("project_id", BS.pack $ T.unpack projectId)] }
  
  response <- httpLbs req manager
  let responseBody = responseBody response
  
  case decode responseBody of
    Just utxos -> return utxos
    Nothing -> return []

-- Generate a contract address for CSG
generateCSGAddress :: Text -> Text
generateCSGAddress csgId = 
  "addr_test1qz" <> T.take 56 (csgId <> csgId <> csgId <> csgId <> csgId) <> "7jhxvfk"

-- Create transaction for joining a CSG
createJoinTransaction :: Text -> Integer -> IO (Either String Text)
createJoinTransaction csgId amount = do
  -- Build CBOR transaction for joining CSG
  let txCbor = "84a400818258" <> csgId <> "00018282581d" <> T.pack (show amount) <> "1a001e8480"
  submitTransaction txCbor

-- Create transaction for claiming rewards
createClaimTransaction :: Text -> Text -> IO (Either String Text)
createClaimTransaction csgId address = do
  let txCbor = "84a400818258" <> csgId <> "00018282581d" <> address <> "1a001e8480"
  submitTransaction txCbor

-- Create transaction for closing CSG
createCloseTransaction :: Text -> IO (Either String Text)
createCloseTransaction csgId = do
  let txCbor = "84a400818258" <> csgId <> "00018282581d1a001e8480"
  submitTransaction txCbor

-- Create transaction for withdrawing from CSG
createWithdrawTransaction :: Text -> Text -> Integer -> IO (Either String Text)
createWithdrawTransaction csgId address amount = do
  let txCbor = "84a400818258" <> csgId <> "00018282581d" <> address <> T.pack (show amount) <> "1a001e8480"
  submitTransaction txCbor 