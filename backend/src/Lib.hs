{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Time
import Data.UUID
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data CSG = CSG
    { csgId :: UUID
    , name :: String
    , maxParticipants :: Int
    , contributionAmount :: Double
    , startDate :: UTCTime
    , endDate :: UTCTime
    } deriving (Eq, Show, Generic)

instance ToJSON CSG
instance FromJSON CSG

type API = "csg" :> Get '[JSON] [CSG]
      :<|> "csg" :> ReqBody '[JSON] CSG :> Post '[JSON] CSG

server :: Server API
server = getCSGs :<|> createCSG
  where
    getCSGs :: Handler [CSG]
    getCSGs = return [] -- In a real app, this would fetch from a database

    createCSG :: CSG -> Handler CSG
    createCSG csg = return csg -- In a real app, this would save to a database

app :: Application
app = serve (Proxy :: Proxy API) server

startApp :: IO ()
startApp = run 8080 app
