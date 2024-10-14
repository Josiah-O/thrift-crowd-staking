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
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as PG
import System.Random (randomRIO)
import Data.Proxy (Proxy(..))

type API = 
  "api" :> 
    (    "create-csg" :> ReqBody '[JSON] CreateCSGRequest :> Post '[JSON] CSG
    :<|> "join-csg" :> Capture "csgId" T.Text :> ReqBody '[JSON] JoinCSGRequest :> Post '[JSON] CSG
    :<|> "claim-reward" :> Capture "csgId" T.Text :> ReqBody '[JSON] ClaimRewardRequest :> Post '[JSON] CSG
    :<|> "close-csg" :> Capture "csgId" T.Text :> Post '[JSON] CSG
    :<|> "list-csgs" :> Get '[JSON] [CSG]
    :<|> "withdraw" :> Capture "csgId" T.Text :> ReqBody '[JSON] WithdrawRequest :> Post '[JSON] CSG
    )

api :: Proxy API
api = Proxy

server :: Connection -> Server API
server conn = createCSG
         :<|> joinCSG
         :<|> claimReward
         :<|> closeCSG
         :<|> listCSGs
         :<|> withdraw
  where
    createCSG :: CreateCSGRequest -> Handler CSG
    createCSG CreateCSGRequest{..} = liftIO $ do
      currentTime <- getCurrentTime
      let endTime = addUTCTime (fromIntegral $ createCsgDuration * 86400) currentTime
      csgId <- T.pack . show <$> (randomRIO (1000, 9999) :: IO Int)
      return $ CSG csgId createCsgName [] createCsgStakeAmount createCsgDuration currentTime endTime "Active"

    joinCSG :: T.Text -> JoinCSGRequest -> Handler CSG
    joinCSG csgId JoinCSGRequest{..} = liftIO $ do
      currentTime <- getCurrentTime
      return $ CSG csgId "Dummy CSG" ["participant1"] joinStakeAmount 0 currentTime currentTime "Active"

    claimReward :: T.Text -> ClaimRewardRequest -> Handler CSG
    claimReward csgId ClaimRewardRequest{..} = liftIO $ do
      currentTime <- getCurrentTime
      return $ CSG csgId "Dummy CSG" [claimantAddress] 0 0 currentTime currentTime "Active"

    closeCSG :: T.Text -> Handler CSG
    closeCSG csgId = liftIO $ do
      currentTime <- getCurrentTime
      return $ CSG csgId "Dummy CSG" [] 0 0 currentTime currentTime "Closed"

    listCSGs :: Handler [CSG]
    listCSGs = liftIO $ do
      currentTime <- getCurrentTime
      return [CSG "csg1" "Dummy CSG" [] 0 0 currentTime currentTime "Active"]

    withdraw :: T.Text -> WithdrawRequest -> Handler CSG
    withdraw csgId WithdrawRequest{..} = liftIO $ do
      currentTime <- getCurrentTime
      return $ CSG csgId "Dummy CSG" [] withdrawAmount 0 currentTime currentTime "Active"

app :: Connection -> Application
app conn = serve api (server conn)

-- Add any additional helper functions or type definitions here
