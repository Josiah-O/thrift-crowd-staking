{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module CSGContract where

-- Unique deployment identifier: Josh-2025-07-17-Fresh-Deploy
import           PlutusTx.Prelude
import qualified PlutusTx
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Scripts       as Scripts
import           Ledger.Ada           as Ada
import           Plutus.V1.Ledger.Api
import           Plutus.V2.Ledger.Contexts

data CSGDatum = CSGDatum
    { csgOwner         :: PubKeyHash
    , csgParticipants  :: [PubKeyHash]
    , csgTotalStake    :: Integer
    , csgMinStake      :: Integer  -- Minimum stake amount set by creator
    , csgDuration      :: POSIXTime
    , csgStartTime     :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''CSGDatum

data CSGRedeemer = Join | Claim | Close
PlutusTx.unstableMakeIsData ''CSGRedeemer

{-# INLINABLE mkValidator #-}
mkValidator :: CSGDatum -> CSGRedeemer -> ScriptContext -> Bool
mkValidator dat red ctx =
    traceIfFalse "Invalid action" $ case red of
        Join  -> validateJoin dat ctx
        Claim -> validateClaim dat ctx
        Close -> validateClose dat ctx

validateJoin :: CSGDatum -> ScriptContext -> Bool
validateJoin dat ctx = 
    traceIfFalse "Insufficient stake amount" sufficientStakeAmount &&
    traceIfFalse "CSG is full" (length (csgParticipants dat) < 10)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sufficientStakeAmount :: Bool
    sufficientStakeAmount = 
        let inVal  = valueSpent info
            outVal = valueProduced info
            stakeAdded = Ada.getLovelace (Ada.fromValue (outVal <> negate inVal))
        in stakeAdded >= csgMinStake dat  -- Check against configurable minimum

validateClaim :: CSGDatum -> ScriptContext -> Bool
validateClaim dat ctx =
    traceIfFalse "Not a participant" isParticipant &&
    traceIfFalse "Too early to claim" canClaim
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signer :: PubKeyHash
    signer = case txInfoSignatories info of
        [pkh] -> pkh
        _     -> traceError "Expected exactly one signer"

    isParticipant :: Bool
    isParticipant = signer `elem` csgParticipants dat

    canClaim :: Bool
    canClaim = from (csgStartTime dat + csgDuration dat) `contains` txInfoValidRange info

validateClose :: CSGDatum -> ScriptContext -> Bool
validateClose dat ctx =
    traceIfFalse "Only owner can close" isOwner &&
    traceIfFalse "Too early to close" canClose
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signer :: PubKeyHash
    signer = case txInfoSignatories info of
        [pkh] -> pkh
        _     -> traceError "Expected exactly one signer"

    isOwner :: Bool
    isOwner = signer == csgOwner dat

    canClose :: Bool
    canClose = from (csgStartTime dat + csgDuration dat) `contains` txInfoValidRange info

typedValidator :: Scripts.TypedValidator CSGDatum
typedValidator = Scripts.mkTypedValidator @CSGDatum
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator