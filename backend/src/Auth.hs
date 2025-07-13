{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth
  ( AuthenticatedUser(..)
  , AuthResult
  , generateJWT
  , validateJWT
  , authHandler
  , authContext
  , requireAuth
  , getUserAddress
  , authenticateWallet
  , User(..)
  , WalletAuthRequest(..)
  , LoginResponse(..)
  , createUserTable
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import GHC.Generics
import Servant
import Servant.Auth.Server
import Control.Monad.IO.Class (liftIO)
import Crypto.BCrypt (hashPasswordUsingPolicy, validatePassword, slowerBcryptHashingPolicy)
import Database.PostgreSQL.Simple (Connection, Only(..))
import qualified Database.PostgreSQL.Simple as PG
import Control.Exception (try, SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

-- User data structure - WALLET ONLY
data User = User
  { userId :: Int
  , userWalletAddress :: Text
  , userCreatedAt :: UTCTime
  } deriving (Show, Generic)

-- Custom JSON instances to match frontend field names
instance ToJSON User where
  toJSON (User uid addr created) = object
    [ "id" .= show uid
    , "walletAddress" .= addr
    , "createdAt" .= created
    ]

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User
    <$> (read <$> o .: "id")
    <*> o .: "walletAddress"
    <*> o .: "createdAt"

-- Authenticated user (for JWT claims) - WALLET ONLY
data AuthenticatedUser = AuthenticatedUser
  { authUserId :: Int
  , authWalletAddress :: Text
  } deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

-- Authentication result type
type AuthResult = AuthResult' AuthenticatedUser

-- Request/Response types - WALLET ONLY
data WalletAuthRequest = WalletAuthRequest
  { walletAddress :: Text
  } deriving (Show, Generic)

instance ToJSON WalletAuthRequest
instance FromJSON WalletAuthRequest

data LoginResponse = LoginResponse
  { loginToken :: Text
  , loginUser :: AuthenticatedUser
  } deriving (Show, Generic)

instance ToJSON LoginResponse
instance FromJSON LoginResponse

-- JWT Configuration
getJWTKey :: IO JWTSettings
getJWTKey = do
  keyEnv <- lookupEnv "JWT_SECRET"
  let secret = fromMaybe "default-secret-key-change-in-production" keyEnv
  key <- generateKey
  return $ defaultJWTSettings key

-- Cookie configuration
getCookieSettings :: CookieSettings
getCookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure }

-- Authentication context
authContext :: IO (Context '[CookieSettings, JWTSettings])
authContext = do
  jwtCfg <- getJWTKey
  return $ getCookieSettings :. jwtCfg :. EmptyContext

-- Auth handler for servant-auth
authHandler :: Connection -> AuthResult -> Handler AuthenticatedUser
authHandler conn (Authenticated user) = return user
authHandler _ (BadPassword) = throwError err401 { errBody = "Invalid password" }
authHandler _ (NoSuchUser) = throwError err401 { errBody = "User not found" }
authHandler _ _ = throwError err401 { errBody = "Authentication required" }

-- Require authentication wrapper
requireAuth :: AuthResult -> Handler AuthenticatedUser
requireAuth (Authenticated user) = return user
requireAuth (BadPassword) = throwError err401 { errBody = "Invalid password" }
requireAuth (NoSuchUser) = throwError err401 { errBody = "User not found" }
requireAuth _ = throwError err401 { errBody = "Authentication required" }

-- Get wallet address from authenticated user
getUserAddress :: AuthenticatedUser -> Text
getUserAddress = authWalletAddress

-- Generate JWT token
generateJWT :: JWTSettings -> AuthenticatedUser -> IO (Maybe Text)
generateJWT jwtCfg user = do
  result <- makeJWT user jwtCfg Nothing
  case result of
    Left _ -> return Nothing
    Right token -> return $ Just $ TE.decodeUtf8 token

-- Validate JWT token
validateJWT :: JWTSettings -> Text -> IO (Maybe AuthenticatedUser)
validateJWT jwtCfg token = do
  result <- verifyJWT jwtCfg (TE.encodeUtf8 token)
  case result of
    Left _ -> return Nothing
    Right user -> return $ Just user

-- Hash password
hashPassword :: Text -> IO (Maybe Text)
hashPassword password = do
  let policy = slowerBcryptHashingPolicy
  hashed <- hashPasswordUsingPolicy policy (TE.encodeUtf8 password)
  return $ fmap TE.decodeUtf8 hashed

-- Verify password
verifyPassword :: Text -> Text -> Bool
verifyPassword password hash = 
  validatePassword (TE.encodeUtf8 hash) (TE.encodeUtf8 password)

-- Register new user - WALLET ONLY
authenticateWallet :: Connection -> WalletAuthRequest -> IO (Either String AuthenticatedUser)
authenticateWallet conn WalletAuthRequest{..} = do
  result <- try $ do
    -- Check if user already exists
    existingUsers <- PG.query conn 
      "SELECT id, wallet_address, created_at FROM users WHERE wallet_address = ?"
      (Only walletAddress)
    
    case existingUsers of
      -- User exists, return existing user
      (userId, userWalletAddr, createdAt):_ -> 
        return $ AuthenticatedUser
          { authUserId = userId
          , authWalletAddress = userWalletAddr
          }
      -- User doesn't exist, create new user
      [] -> do
        -- Insert new user
        [userId] <- PG.query conn 
          "INSERT INTO users (wallet_address) VALUES (?) RETURNING id"
          (Only walletAddress)
        
        return $ AuthenticatedUser
          { authUserId = userId
          , authWalletAddress = walletAddress
          }
  
  case result of
    Left ex -> return $ Left $ show ex
    Right user -> return $ Right user

-- Remove old login/register functions - replaced with single wallet authentication
-- loginUser :: Connection -> LoginRequest -> IO (Either String AuthenticatedUser)
-- registerUser :: Connection -> RegisterRequest -> IO (Either String AuthenticatedUser)

-- Database migration (call this on startup) - WALLET ONLY
createUserTable :: Connection -> IO ()
createUserTable conn = do
  _ <- PG.execute_ conn $ 
    "CREATE TABLE IF NOT EXISTS users (" <>
    "  id SERIAL PRIMARY KEY," <>
    "  wallet_address TEXT NOT NULL UNIQUE," <>
    "  created_at TIMESTAMP NOT NULL DEFAULT NOW()" <>
    ")"
  return () 