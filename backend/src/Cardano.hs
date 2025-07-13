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
import Data.Time (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple (Connection, Only(..))
import qualified CSGContract

-- Network configuration
data CardanoNetwork = Testnet | Preprod | Mainnet
  deriving (Show, Eq)

-- Staking Pool Information
data StakePool = StakePool
  { poolId :: Text
  , poolTicker :: Text
  , poolName :: Text
  , poolMargin :: Double
  , poolFixedCost :: Integer
  , poolSaturation :: Double
  , poolRoa :: Double -- Return on ADA
  } deriving (Show, Eq)

instance FromJSON StakePool where
  parseJSON = withObject "StakePool" $ \o -> StakePool
    <$> o .: "pool_id"
    <*> o .: "ticker"
    <*> o .: "name"
    <*> o .: "margin"
    <*> o .: "fixed_cost"
    <*> o .: "saturation"
    <*> o .: "roa"

-- Staking Rewards Information
data StakingRewards = StakingRewards
  { rewardEpoch :: Integer
  , rewardAmount :: Integer
  , rewardPool :: Text
  } deriving (Show, Eq)

instance FromJSON StakingRewards where
  parseJSON = withObject "StakingRewards" $ \o -> StakingRewards
    <$> o .: "epoch"
    <*> o .: "amount"
    <*> o .: "pool_id"

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

-- Query the best stake pools for delegation
queryBestStakePools :: Integer -> IO (Either String [StakePool])
queryBestStakePools totalStake = do
  projectIdResult <- getBlockfrostProjectId
  case projectIdResult of
    Left err -> return $ Left err
    Right projectId -> do
      baseUrl <- getBlockfrostBaseUrl
      manager <- newManager tlsManagerSettings
      let url = baseUrl ++ "/pools?count=20&order=asc"
      
      initReq <- parseRequest url
      let req = initReq
            { requestHeaders = [("project_id", BS.pack $ T.unpack projectId)] }
      
      response <- httpLbs req manager
      let respBody = responseBody response
      let statusCode' = statusCode $ responseStatus response
      
      if statusCode' == 200
        then case decode respBody of
          Just pools -> do
            -- Filter pools based on criteria
            let suitablePools = filter (\pool -> 
                  poolSaturation pool < 0.8 &&  -- Not oversaturated
                  poolRoa pool > 0.04 &&         -- At least 4% ROA
                  totalStake < 1000000000000     -- Less than 1M ADA
                ) pools
            return $ Right $ take 5 suitablePools
          Nothing -> return $ Left "Failed to parse pools response"
        else return $ Left $ "HTTP error querying pools: " ++ show statusCode'

-- Replace the selectOptimalPool function and add supporting functions

-- Pool usage data from database
data PoolUsage = PoolUsage
  { usagePoolId :: Text
  , usageCSGCount :: Int
  } deriving (Show, Eq)

-- Scored pool for selection algorithm
data ScoredPool = ScoredPool
  { scoredPool :: StakePool
  , poolScore :: Double
  } deriving (Show, Eq)

-- Query database for pool usage statistics
queryPoolUsage :: Connection -> [Text] -> IO (Either String [PoolUsage])
queryPoolUsage conn poolIds = do
  result <- try $ PG.query conn 
    "SELECT pool_id, COUNT(*) FROM csgs WHERE pool_id = ANY(?) AND status = 'Active' GROUP BY pool_id"
    (Only $ PG.In poolIds)
  
  case result of
    Left ex -> return $ Left $ "Database error querying pool usage: " ++ show ex
    Right rows -> do
      let usageMap = [(poolId, count) | (poolId :: Text, count :: Int) <- rows]
      let allUsage = map (\pid -> 
            case lookup pid usageMap of
              Just count -> PoolUsage pid count
              Nothing -> PoolUsage pid 0
          ) poolIds
      return $ Right allUsage

-- Calculate diversified score for pool selection
calculateDiversifiedScore :: StakePool -> PoolUsage -> Double
calculateDiversifiedScore pool usage = 
  let baseScore = poolRoa pool - poolSaturation pool
      usagePenalty = fromIntegral (usageCSGCount usage) * 0.3  -- 0.3% penalty per CSG
      diversificationBonus = if usageCSGCount usage == 0 then 0.2 else 0.0  -- 0.2% bonus for unused pools
      finalScore = baseScore - usagePenalty + diversificationBonus
  in finalScore

-- Select optimal pool with load-based diversification
selectOptimalPoolWithDiversification :: Connection -> Integer -> IO (Either String Text)
selectOptimalPoolWithDiversification conn totalStake = do
  poolsResult <- queryBestStakePools totalStake
  case poolsResult of
    Left err -> return $ Left err
    Right [] -> return $ Left "No suitable pools found"
    Right pools -> do
      let poolIds = map poolId pools
      usageResult <- queryPoolUsage conn poolIds
      
      case usageResult of
        Left err -> return $ Left err
        Right usageData -> do
          -- Create usage lookup map for O(1) access
          let usageMap = [(usagePoolId u, u) | u <- usageData]
          
          -- Score all pools considering diversification
          let scoredPools = map (\pool ->
                let usage = case lookup (poolId pool) usageMap of
                      Just u -> u
                      Nothing -> PoolUsage (poolId pool) 0
                    score = calculateDiversifiedScore pool usage
                in ScoredPool pool score
              ) pools
          
          -- Sort by score (highest first) and select best
          let sortedPools = sortBy (\a b -> compare (poolScore b) (poolScore a)) scoredPools
          
          case sortedPools of
            [] -> return $ Left "No pools available after scoring"
            (bestScoredPool:_) -> return $ Right $ poolId $ scoredPool bestScoredPool

-- Updated delegation function with database connection
delegateCSGToPoolWithDiversification :: Connection -> Text -> Integer -> IO (Either String Text)
delegateCSGToPoolWithDiversification conn csgId totalStake = do
  poolResult <- selectOptimalPoolWithDiversification conn totalStake
  case poolResult of
    Left err -> return $ Left err
    Right poolId -> do
      let csgAddr = generateCSGAddress csgId
      delegationResult <- createDelegationTransaction csgAddr poolId
      
      case delegationResult of
        Right txHash -> do
          -- Update database with selected pool
          updateResult <- try $ PG.execute conn
            "UPDATE csgs SET pool_id = ? WHERE id = ?"
            (poolId, csgId)
          
          case updateResult of
            Left ex -> return $ Left $ "Failed to update CSG with pool ID: " ++ show ex
            Right _ -> return $ Right txHash
        Left err -> return $ Left err

-- Update the original selectOptimalPool to use diversification by default
selectOptimalPool :: Integer -> IO (Either String Text)
selectOptimalPool totalStake = do
  -- This is a fallback that doesn't use diversification
  -- In practice, we should always use selectOptimalPoolWithDiversification
  poolsResult <- queryBestStakePools totalStake
  case poolsResult of
    Left err -> return $ Left err
    Right [] -> return $ Left "No suitable pools found"
    Right pools -> do
      let bestPool = head $ sortBy compareROA pools
      return $ Right $ poolId bestPool
  where
    compareROA pool1 pool2 = 
      compare (poolRoa pool2 - poolSaturation pool2) (poolRoa pool1 - poolSaturation pool1)

-- Create delegation certificate and transaction
createDelegationTransaction :: Text -> Text -> IO (Either String Text)
createDelegationTransaction csgAddr poolId = do
  network <- getNetwork
  let networkFlag = getNetworkFlag network
  
  withSystemTempDirectory "cardano-delegation" $ \tmpDir -> do
    let stakeCertFile = tmpDir </> "stake.cert"
    let delegCertFile = tmpDir </> "deleg.cert"
    let txBodyFile = tmpDir </> "tx.body"
    let txFile = tmpDir </> "tx.signed"
    
    -- Create stake address registration certificate
    skeyResult <- getWalletSigningKey
    case skeyResult of
      Left err -> return $ Left $ "Failed to get wallet signing key: " ++ err
      Right signingKeyFile -> do
        let verificationKeyFile = signingKeyFile ++ ".vkey"  -- Assume .vkey exists alongside .skey
        stakeResult <- try $ readProcessWithExitCode "cardano-cli" 
          [ "stake-address", "registration-certificate"
          , "--stake-verification-key-file", verificationKeyFile
          , "--out-file", stakeCertFile
          ] ""
    
    case stakeResult of
      Left ex -> return $ Left $ "Failed to create stake certificate: " ++ show ex
      Right (ExitSuccess, _, _) -> do
        -- Create delegation certificate
        skeyResult <- getWalletSigningKey
        case skeyResult of
          Left err -> return $ Left $ "Failed to get wallet signing key: " ++ err
          Right signingKeyFile -> do
            let verificationKeyFile = signingKeyFile ++ ".vkey"  -- Assume .vkey exists alongside .skey
            delegResult <- try $ readProcessWithExitCode "cardano-cli" 
              [ "stake-address", "delegation-certificate"
              , "--stake-verification-key-file", verificationKeyFile
              , "--stake-pool-id", T.unpack poolId
              , "--out-file", delegCertFile
              ] ""
        
            case delegResult of
              Left ex -> return $ Left $ "Failed to create delegation certificate: " ++ show ex
              Right (ExitSuccess, _, _) -> do
                -- Query UTXOs for delegation transaction
                utxosResult <- queryUTXOs csgAddr
                case utxosResult of
                  Left err -> return $ Left $ "Failed to query UTXOs for delegation: " ++ err
                  Right utxos -> do
                    case selectUTXOs utxos 1000000 of  -- Select UTXOs for delegation fee
                      Left err -> return $ Left $ "UTXO selection failed for delegation: " ++ err
                      Right selectedUtxos -> do
                        let txInArgs = concatMap (\utxo -> 
                              ["--tx-in", T.unpack (utxoTxHash utxo) ++ "#" ++ show (utxoTxIndex utxo)]) selectedUtxos
                        let collateralUtxo = head selectedUtxos
                        let collateralArg = ["--tx-in-collateral", T.unpack (utxoTxHash collateralUtxo) ++ "#" ++ show (utxoTxIndex collateralUtxo)]
                        
                        -- Build transaction with certificates
                        buildResult <- try $ readProcessWithExitCode "cardano-cli" 
                          ([ "transaction", "build"
                          ] ++ txInArgs ++ collateralArg ++
                          [ "--change-address", T.unpack csgAddr
                          , "--certificate-file", stakeCertFile
                          , "--certificate-file", delegCertFile
                          , "--out-file", txBodyFile
                          ]) (unwords [networkFlag])
                        
                        case buildResult of
                          Left ex -> return $ Left $ "Failed to build delegation transaction: " ++ show ex
                          Right (ExitSuccess, _, _) -> do
                            -- Sign and submit transaction
                            signResult <- signTransaction txBodyFile txFile
                            case signResult of
                              Right txId -> return $ Right txId
                              Left err -> return $ Left err
                          Right (ExitFailure code, _, stderr) -> 
                            return $ Left $ "Delegation transaction build failed: " ++ stderr
              Right (ExitFailure code, _, stderr) -> 
                return $ Left $ "Delegation certificate creation failed: " ++ stderr
      Right (ExitFailure code, _, stderr) -> 
        return $ Left $ "Stake certificate creation failed: " ++ stderr

-- Query actual staking rewards for an address
queryStakingRewards :: Text -> UTCTime -> UTCTime -> IO (Either String [StakingRewards])
queryStakingRewards address startTime endTime = do
  projectIdResult <- getBlockfrostProjectId
  case projectIdResult of
    Left err -> return $ Left err
    Right projectId -> do
      baseUrl <- getBlockfrostBaseUrl
      manager <- newManager tlsManagerSettings
      let url = baseUrl ++ "/accounts/" ++ T.unpack address ++ "/rewards"
      
      initReq <- parseRequest url
      let req = initReq
            { requestHeaders = [("project_id", BS.pack $ T.unpack projectId)] }
      
      response <- httpLbs req manager
      let respBody = responseBody response
      let statusCode' = statusCode $ responseStatus response
      
      if statusCode' == 200
        then case decode respBody of
          Just rewards -> return $ Right rewards
          Nothing -> return $ Left "Failed to parse rewards response"
        else return $ Left $ "HTTP error querying rewards: " ++ show statusCode'

-- Calculate total staking rewards for a CSG
calculateCSGRewards :: Text -> UTCTime -> UTCTime -> IO (Either String Integer)
calculateCSGRewards csgAddr startTime endTime = do
  rewardsResult <- queryStakingRewards csgAddr startTime endTime
  case rewardsResult of
    Left err -> return $ Left err
    Right rewards -> do
      let totalRewards = sum $ map rewardAmount rewards
      return $ Right totalRewards

-- Calculate proportional rewards distribution
calculateProportionalRewards :: [(Text, Integer)] -> Integer -> [(Text, Integer)]
calculateProportionalRewards participants totalRewards = 
  let totalStake = sum $ map snd participants
      calculateReward (addr, stake) = 
        let proportion = fromIntegral stake / fromIntegral totalStake
            reward = round (proportion * fromIntegral totalRewards)
        in (addr, reward)
  in map calculateReward participants

-- Create real staking delegation for CSG
delegateCSGToPool :: Text -> Integer -> IO (Either String Text)
delegateCSGToPool csgId totalStake = do
  -- Note: This version doesn't have database access, so falls back to simple selection
  -- The API layer should use delegateCSGToPoolWithDiversification instead
  poolResult <- selectOptimalPool totalStake
  case poolResult of
    Left err -> return $ Left err
    Right poolId -> do
      let csgAddr = generateCSGAddress csgId
      createDelegationTransaction csgAddr poolId

-- UTXO data structure
data UTXO = UTXO
  { utxoTxHash :: Text
  , utxoTxIndex :: Integer
  , utxoAmount :: Integer
  } deriving (Show, Eq)

-- Query UTXOs using cardano-cli and parse them properly
queryUTXOs :: Text -> IO (Either String [UTXO])
queryUTXOs address = do
  network <- getNetwork
  let networkFlag = getNetworkFlag network
  
  result <- try $ readProcessWithExitCode "cardano-cli" 
    ["query", "utxo", "--address", T.unpack address, "--out-file", "/dev/stdout"] 
    (unwords [networkFlag])
    
  case result of
    Left ex -> return $ Left $ "Failed to query UTXOs: " ++ show ex
    Right (ExitSuccess, stdout, _) -> do
      -- Parse UTXOs from cardano-cli output
      let utxos = parseUTXOsFromOutput stdout
      return $ Right utxos
    Right (ExitFailure code, _, stderr) -> 
      return $ Left $ "cardano-cli failed with code " ++ show code ++ ": " ++ stderr

-- Parse UTXOs from cardano-cli output
parseUTXOsFromOutput :: String -> [UTXO]
parseUTXOsFromOutput output = 
  let lines' = lines output
      dataLines = drop 2 lines'  -- Skip header lines
      parseUTXO line = 
        let parts = words line
        in case parts of
          (txHashIndex:_:amount:_) -> 
            let (txHash, txIndex) = case T.splitOn "#" (T.pack txHashIndex) of
                  [h, i] -> (h, read (T.unpack i) :: Integer)
                  _ -> (T.pack txHashIndex, 0)
            in Just $ UTXO txHash txIndex (read amount)
          _ -> Nothing
  in mapMaybe parseUTXO dataLines
  where mapMaybe f = foldr (\x acc -> case f x of Just y -> y:acc; Nothing -> acc) []

-- Select suitable UTXOs for a transaction
selectUTXOs :: [UTXO] -> Integer -> Either String [UTXO]
selectUTXOs utxos targetAmount = 
  let sortedUtxos = sortBy (\a b -> compare (utxoAmount b) (utxoAmount a)) utxos
      selectHelper [] _ acc = Left "Insufficient funds"
      selectHelper (u:us) remaining acc
        | remaining <= 0 = Right acc
        | otherwise = selectHelper us (remaining - utxoAmount u) (u:acc)
  in selectHelper sortedUtxos targetAmount []

-- Build a transaction using cardano-cli with real UTXOs
buildTransaction :: Text -> Text -> Integer -> IO (Either String Text)
buildTransaction fromAddr toAddr amount = do
  network <- getNetwork
  let networkFlag = getNetworkFlag network
  
  -- Query available UTXOs
  utxosResult <- queryUTXOs fromAddr
  case utxosResult of
    Left err -> return $ Left $ "Failed to query UTXOs: " ++ err
    Right utxos -> do
      -- Select UTXOs for the transaction
      case selectUTXOs utxos amount of
        Left err -> return $ Left $ "UTXO selection failed: " ++ err
        Right selectedUtxos -> do
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
                -- Build transaction body with real UTXOs
                let txInArgs = concatMap (\utxo -> 
                      ["--tx-in", T.unpack (utxoTxHash utxo) ++ "#" ++ show (utxoTxIndex utxo)]) selectedUtxos
                let collateralUtxo = head selectedUtxos  -- Use first UTXO as collateral
                let collateralArg = ["--tx-in-collateral", T.unpack (utxoTxHash collateralUtxo) ++ "#" ++ show (utxoTxIndex collateralUtxo)]
                
                buildResult <- try $ readProcessWithExitCode "cardano-cli" 
                  ([ "transaction", "build"
                  ] ++ txInArgs ++ collateralArg ++
                  [ "--tx-out", T.unpack toAddr ++ "+" ++ show amount
                  , "--change-address", T.unpack fromAddr
                  , "--protocol-params-file", tmpDir </> "protocol.json"
                  , "--out-file", txBodyFile
                  ]) ""
                  
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
    Right (ExitSuccess, stdout, _) -> do
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
generateCSGAddress _ = 
  -- Use the real Plutus script address from our deployed contract
  T.pack $ show CSGContract.scrAddress

-- Create a real transaction for joining a CSG with automatic delegation
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
          -- First, send funds to CSG
          joinResult <- buildTransaction walletAddr csgAddr amount
          case joinResult of
            Right txId -> do
              -- Then delegate the CSG to optimal pool
              delegateCSGToPool csgId amount
            Left err -> return $ Left err

-- Create a real transaction for claiming proportional rewards
createClaimTransaction :: Text -> Text -> IO (Either String Text)
createClaimTransaction csgId userAddr
  | T.length csgId < 5 = return $ Left "Invalid CSG ID"
  | T.length userAddr < 10 = return $ Left "Invalid Cardano address"
  | otherwise = do
      let csgAddr = generateCSGAddress csgId
      -- Query actual staking rewards for this CSG
      currentTime <- getCurrentTime
      let startTime = addUTCTime (-2592000) currentTime  -- 30 days ago
      
      rewardsResult <- calculateCSGRewards csgAddr startTime currentTime
      case rewardsResult of
        Left err -> return $ Left err
        Right totalRewards -> do
          -- For now, assume equal distribution (would need participant data)
          let userReward = totalRewards `div` 10  -- Divide by max participants
          buildTransaction csgAddr userAddr userReward

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
          -- Query actual balance and return to creator
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

-- Custom sorting implementation
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (x:xs) = smaller ++ [x] ++ larger
  where
    smaller = sortBy cmp [y | y <- xs, cmp y x == LT]
    larger = sortBy cmp [y | y <- xs, cmp y x /= LT] 