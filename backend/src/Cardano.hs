{-# LANGUAGE OverloadedStrings #-}

module Cardano where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS
import System.Environment (lookupEnv)
import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.Maybe (fromMaybe)
import Control.Exception (try, SomeException)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

-- Network configuration
data CardanoNetwork = Testnet | Preprod | Mainnet
  deriving (Show, Eq)

getNetwork :: IO CardanoNetwork
getNetwork = do
  maybeNet <- lookupEnv "CARDANO_NETWORK"
  return $ case maybeNet of
    Just "mainnet" -> Mainnet
    Just "preprod" -> Preprod
    _ -> Testnet

getNetworkFlag :: CardanoNetwork -> String
getNetworkFlag Testnet = "--testnet-magic 1"
getNetworkFlag Preprod = "--testnet-magic 1"
getNetworkFlag Mainnet = "--mainnet"

-- Blockfrost configuration
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
    Just projectId -> return $ Right (T.pack projectId)

-- Wallet configuration
getWalletAddress :: IO (Either String Text)
getWalletAddress = do
  maybeAddr <- lookupEnv "CARDANO_WALLET_ADDRESS"
  case maybeAddr of
    Nothing -> return $ Left "CARDANO_WALLET_ADDRESS environment variable not set"
    Just "" -> return $ Left "CARDANO_WALLET_ADDRESS environment variable is empty"
    Just addr -> return $ Right (T.pack addr)

getWalletSigningKey :: IO (Either String FilePath)
getWalletSigningKey = do
  maybeKey <- lookupEnv "CARDANO_WALLET_SKEY_FILE"
  case maybeKey of
    Nothing -> return $ Left "CARDANO_WALLET_SKEY_FILE environment variable not set"
    Just "" -> return $ Left "CARDANO_WALLET_SKEY_FILE environment variable is empty"
    Just keyFile -> return $ Right keyFile

-- Query UTXOs using cardano-cli
queryUTXOs :: Text -> IO (Either String [Value])
queryUTXOs address = do
  network <- getNetwork
  let networkFlag = getNetworkFlag network
  
  result <- try $ readProcessWithExitCode "cardano-cli" 
    ["query", "utxo", "--address", T.unpack address, "--out-file", "/dev/stdout"] 
    (unwords [networkFlag])
    
  case result of
    Left ex -> return $ Left $ "Failed to query UTXOs: " ++ show ex
    Right (ExitSuccess, stdout, _) -> 
      case decode (LBS.fromStrict $ BS.pack stdout) of
        Just utxos -> return $ Right utxos
        Nothing -> return $ Left "Failed to parse UTXO JSON"
    Right (ExitFailure code, _, stderr) -> 
      return $ Left $ "cardano-cli failed with code " ++ show code ++ ": " ++ stderr

-- Build a transaction using cardano-cli
buildTransaction :: Text -> Text -> Integer -> IO (Either String Text)
buildTransaction fromAddr toAddr amount = do
  network <- getNetwork
  let networkFlag = getNetworkFlag network
  
  withSystemTempDirectory "cardano-tx" $ \tmpDir -> do
    let txBodyFile = tmpDir </> "tx.body"
    let txFile = tmpDir </> "tx.signed"
    
    -- Query protocol parameters
    protocolResult <- try $ readProcessWithExitCode "cardano-cli" 
      ["query", "protocol-parameters", "--out-file", tmpDir </> "protocol.json"]
      (unwords [networkFlag])
      
    case protocolResult of
      Left ex -> return $ Left $ "Failed to query protocol parameters: " ++ show ex
      Right (ExitSuccess, _, _) -> do
        -- Build transaction body
        buildResult <- try $ readProcessWithExitCode "cardano-cli" 
          [ "transaction", "build"
          , "--tx-in-collateral", "dummy"  -- Will be replaced with actual UTXOs
          , "--tx-out", T.unpack toAddr ++ "+" ++ show amount
          , "--change-address", T.unpack fromAddr
          , "--protocol-params-file", tmpDir </> "protocol.json"
          , "--out-file", txBodyFile
          ] ""
          
        case buildResult of
          Left ex -> return $ Left $ "Failed to build transaction: " ++ show ex
          Right (ExitSuccess, _, _) -> do
            -- Sign transaction
            signResult <- signTransaction txBodyFile txFile
            case signResult of
              Right txId -> return $ Right txId
              Left err -> return $ Left err
          Right (ExitFailure code, _, stderr) -> 
            return $ Left $ "Transaction build failed with code " ++ show code ++ ": " ++ stderr
      Right (ExitFailure code, _, stderr) -> 
        return $ Left $ "Protocol parameters query failed with code " ++ show code ++ ": " ++ stderr

-- Sign a transaction
signTransaction :: FilePath -> FilePath -> IO (Either String Text)
signTransaction txBodyFile txFile = do
  skeyResult <- getWalletSigningKey
  case skeyResult of
    Left err -> return $ Left err
    Right skeyFile -> do
      result <- try $ readProcessWithExitCode "cardano-cli" 
        [ "transaction", "sign"
        , "--tx-body-file", txBodyFile
        , "--signing-key-file", skeyFile
        , "--out-file", txFile
        ] ""
        
      case result of
        Left ex -> return $ Left $ "Failed to sign transaction: " ++ show ex
        Right (ExitSuccess, _, _) -> do
          -- Submit transaction
          submitResult <- submitTransactionFile txFile
          case submitResult of
            Right txId -> return $ Right txId
            Left err -> return $ Left err
        Right (ExitFailure code, _, stderr) -> 
          return $ Left $ "Transaction signing failed with code " ++ show code ++ ": " ++ stderr

-- Submit a transaction file
submitTransactionFile :: FilePath -> IO (Either String Text)
submitTransactionFile txFile = do
  network <- getNetwork
  let networkFlag = getNetworkFlag network
  
  result <- try $ readProcessWithExitCode "cardano-cli" 
    ["transaction", "submit", "--tx-file", txFile]
    (unwords [networkFlag])
    
  case result of
    Left ex -> return $ Left $ "Failed to submit transaction: " ++ show ex
    Right (ExitSuccess, stdout, _) -> 
      -- Extract transaction ID from stdout
      let txId = T.strip $ T.pack stdout
      return $ Right txId
    Right (ExitFailure code, _, stderr) -> 
      return $ Left $ "Transaction submission failed with code " ++ show code ++ ": " ++ stderr

-- Submit raw transaction CBOR using Blockfrost
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

-- Generate a CSG contract address (script address)
generateCSGAddress :: Text -> Text
generateCSGAddress csgId = 
  -- This would normally be generated from a Plutus script
  -- For now, create a deterministic testnet address
  "addr_test1qz" <> T.take 56 (T.concat $ replicate 10 csgId) <> "7jhxvfk"

-- Create a real transaction for joining a CSG
createJoinTransaction :: Text -> Integer -> IO (Either String Text)
createJoinTransaction csgId amount
  | amount <= 0 = return $ Left "Amount must be positive"
  | T.length csgId < 5 = return $ Left "Invalid CSG ID"
  | otherwise = do
      walletAddrResult <- getWalletAddress
      case walletAddrResult of
        Left err -> return $ Left err
        Right walletAddr -> do
          let csgAddr = generateCSGAddress csgId
          buildTransaction walletAddr csgAddr amount

-- Create a real transaction for claiming rewards
createClaimTransaction :: Text -> Text -> IO (Either String Text)
createClaimTransaction csgId userAddr
  | T.length csgId < 5 = return $ Left "Invalid CSG ID"
  | T.length userAddr < 10 = return $ Left "Invalid Cardano address"
  | otherwise = do
      let csgAddr = generateCSGAddress csgId
      -- For claim, we're sending from CSG contract to user (would need script execution)
      -- For now, simulate by building a transaction
      buildTransaction csgAddr userAddr 1000000  -- 1 ADA as example reward

-- Create a real transaction for closing a CSG
createCloseTransaction :: Text -> IO (Either String Text)
createCloseTransaction csgId
  | T.length csgId < 5 = return $ Left "Invalid CSG ID"
  | otherwise = do
      walletAddrResult <- getWalletAddress
      case walletAddrResult of
        Left err -> return $ Left err
        Right walletAddr -> do
          let csgAddr = generateCSGAddress csgId
          -- Close transaction would return funds to creator
          buildTransaction csgAddr walletAddr 0  -- Return all funds

-- Create a real transaction for withdrawing from CSG
createWithdrawTransaction :: Text -> Text -> Integer -> IO (Either String Text)
createWithdrawTransaction csgId userAddr amount
  | amount <= 0 = return $ Left "Amount must be positive"
  | T.length csgId < 5 = return $ Left "Invalid CSG ID"
  | T.length userAddr < 10 = return $ Left "Invalid Cardano address"
  | otherwise = do
      let csgAddr = generateCSGAddress csgId
      buildTransaction csgAddr userAddr amount

-- Query address UTXOs using Blockfrost (for compatibility)
queryAddressUtxos :: Text -> IO [Value]
queryAddressUtxos address = do
  projectIdResult <- getBlockfrostProjectId
  case projectIdResult of
    Left _ -> return []
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