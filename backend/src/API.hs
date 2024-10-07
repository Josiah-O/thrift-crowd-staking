{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module API where

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
import Crypto.Hash (SHA256, hash)
import Data.ByteString.Char8 (pack)
import Control.Monad.Except (throwError)
import Servant.Server (err404, err400, err500)
import Data.Validation

-- ... (rest of the file remains the same)
