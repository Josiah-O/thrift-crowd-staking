{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import API (app)
import qualified CSG
import Database.PostgreSQL.Simple
import System.Environment (getEnv)
import Control.Exception (catch, SomeException)
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "Starting Thrift Crowd Staking backend on port 8080..."
  catch (do
    dbUrl <- getEnv "DATABASE_URL"
    conn <- connectPostgreSQL dbUrl
    run 8080 $ simpleCors $ app conn
  ) (\e -> do
    putStrLn $ "Error: " ++ show (e :: SomeException)
    exitFailure
  )
