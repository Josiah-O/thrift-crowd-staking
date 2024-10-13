{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Database.PostgreSQL.Simple
import API (app)
import CSG (CSG(..), CreateCSGRequest(..))
import Data.Text (Text)
import Data.Time (UTCTime)

-- Remove or comment out the duplicate data definitions
-- data CSG = CSG
--   { csgId :: Text
--   , csgName :: Text
--   ...
--   }

-- data CreateCSGRequest = CreateCSGRequest
--   { csgName :: Text
--   , csgDuration :: Integer
--   , csgStakeAmount :: Integer
--   }

main :: IO ()
main = do
  putStrLn "Starting Thrift Crowd Staking backend on port 8080..."
  conn <- connectPostgreSQL "host=db dbname=thrift_crowd_staking user=postgres password=password"
  run 8080 $ app conn
