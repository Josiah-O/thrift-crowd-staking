{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module CSGContract where

import           PlutusTx.Prelude
import qualified PlutusTx
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value         as Value
import           Ledger.Typed.Scripts as Scripts
import           Plutus.V1.Ledger.Interval (contains, from, to)

data CSG = CSG
    { csgId            :: BuiltinByteString
    , csgParticipants  :: [(PubKeyHash, Integer)]
    , csgCycleNumber   :: Integer
    , csgWeek          :: Integer
    , csgTotalAmount   :: Integer
    , csgRewardPool    :: Integer
    , csgActive        :: Bool
    , csgMaxParticipants :: Integer
    , csgContributionAmount :: Integer
    , csgStartTime     :: POSIXTime
    , csgEndTime       :: POSIXTime
    }

PlutusTx.makeLift ''CSG

data CSGAction = Join PubKeyHash Integer
               | ClaimReward PubKeyHash
               | EndCycle
               | Withdraw PubKeyHash Integer

PlutusTx.makeLift ''CSGAction

{-# INLINABLE validateCSG #-}
validateCSG :: CSG -> CSGAction -> ScriptContext -> Bool
validateCSG csg action ctx =
    case action of
        Join pkh amount   -> validateJoin csg pkh amount ctx
        ClaimReward pkh   -> validateClaimReward csg pkh ctx
        EndCycle          -> validateEndCycle csg ctx
        Withdraw pkh amount -> validateWithdraw csg pkh amount ctx

{-# INLINABLE validateJoin #-}
validateJoin :: CSG -> PubKeyHash -> Integer -> ScriptContext -> Bool
validateJoin csg pkh amount ctx =
    traceIfFalse "CSG is not active" (csgActive csg) &&
    traceIfFalse "Max participants reached" (length (csgParticipants csg) < fromInteger (csgMaxParticipants csg)) &&
    traceIfFalse "Incorrect contribution amount" (amount == csgContributionAmount csg) &&
    traceIfFalse "Joining period has ended" (validateJoinTime (csgStartTime csg) (csgEndTime csg) (txInfoValidRange $ scriptContextTxInfo ctx)) &&
    traceIfFalse "Participant already joined" (not $ any ((== pkh) . fst) (csgParticipants csg))

{-# INLINABLE validateClaimReward #-}
validateClaimReward :: CSG -> PubKeyHash -> ScriptContext -> Bool
validateClaimReward csg pkh ctx =
    traceIfFalse "Not a participant" (any ((== pkh) . fst) (csgParticipants csg)) &&
    traceIfFalse "Incorrect claim time" (validateClaimTime (csgWeek csg) (txInfoValidRange $ scriptContextTxInfo ctx)) &&
    traceIfFalse "No rewards to claim" (csgRewardPool csg > 0)

{-# INLINABLE validateEndCycle #-}
validateEndCycle :: CSG -> ScriptContext -> Bool
validateEndCycle csg ctx =
    traceIfFalse "Cycle not complete" (csgWeek csg >= 52) &&
    traceIfFalse "Incorrect end time" (validateEndTime (csgEndTime csg) (txInfoValidRange $ scriptContextTxInfo ctx)) &&
    traceIfFalse "CSG is not active" (csgActive csg)

{-# INLINABLE validateWithdraw #-}
validateWithdraw :: CSG -> PubKeyHash -> Integer -> ScriptContext -> Bool
validateWithdraw csg pkh amount ctx =
    traceIfFalse "Not a participant" (any ((== pkh) . fst) (csgParticipants csg)) &&
    traceIfFalse "Insufficient funds" (amount <= participantBalance pkh (csgParticipants csg)) &&
    traceIfFalse "CSG is still active" (not $ csgActive csg)

{-# INLINABLE validateJoinTime #-}
validateJoinTime :: POSIXTime -> POSIXTime -> ValidityInterval -> Bool
validateJoinTime start end interval =
    interval contains from start && interval contains to end

{-# INLINABLE validateClaimTime #-}
validateClaimTime :: Integer -> ValidityInterval -> Bool
validateClaimTime week interval =
    let claimStart = POSIXTime $ 1000000 * 60 * 60 * 24 * 7 * (week - 1)  -- Start of the week
        claimEnd   = POSIXTime $ 1000000 * 60 * 60 * 24 * 7 * week        -- End of the week
    in interval contains from claimStart && interval contains to claimEnd

{-# INLINABLE validateEndTime #-}
validateEndTime :: POSIXTime -> ValidityInterval -> Bool
validateEndTime endTime interval = interval contains from endTime

{-# INLINABLE participantBalance #-}
participantBalance :: PubKeyHash -> [(PubKeyHash, Integer)] -> Integer
participantBalance pkh = foldr (\(p, bal) acc -> if p == pkh then bal + acc else acc) 0

data CSGTypes
instance Scripts.ValidatorTypes CSGTypes where
    type instance DatumType CSGTypes = CSG
    type instance RedeemerType CSGTypes = CSGAction

typedValidator :: Scripts.TypedValidator CSGTypes
typedValidator = Scripts.mkTypedValidator @CSGTypes
    tasks.md(PlutusTx.compile [|| validateCSG ||])
    tasks.md(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @CSG @CSGAction

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator
