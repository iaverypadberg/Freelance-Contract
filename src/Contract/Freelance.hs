-- Problem Statement: Freelancers cannot rely on employers to pay them the agreed upon amount, on time.
-- Solution: This contact will allow freelancers and employers to plan out their payment amount
-- and payment interval. This eliminates freelancers waiting for payments and protects each side from the other.

-- High Level Contract Structure: A State Machine which is very dependent on time.
-- Low Level Contract Structure: 
    -- Number of parties: 2 - Employer & Freelancer
    -- Endpoints: 

        -- Accept: Freelancer accepts the contract
        --  Claim: Freelancer claims their available funds from the contract
        --    End: Freelancer or Employer wish to end the contract 


{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Contract.Freelance
    ( FreelanceContract (..)
    , EmployerParams (..)
    , FStartSchema
    , FContractSchema
    , Last (..)
    , ThreadToken
    , Text
    , startEndpoints
    , endpoints
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

-- Freelance Contract Parameters
-- TODO: Possibly add gracePeriod as a parameter
-- TODO: Update contract log so it responds correctly once the transition function has been executed
data FreelanceContract = FreelanceContract{
    employer            :: !PubKeyHash,
    freelancer          :: !PubKeyHash,
    numberOfIntervals   :: !Integer,
    intervalLength      :: !Integer,
    totalPayment        :: !Integer,
    startTime           :: !POSIXTime,
    signDeadline        :: !POSIXTime,
    cToken              :: !(Maybe ThreadToken)
}deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''FreelanceContract

-- Datum is the # of finished intervals & the claimed value
data FContractDatum = FContractDatum Integer Integer | Finished
    deriving Show
PlutusTx.unstableMakeIsData ''FContractDatum

-- Redeemer for interacting with the contract
data FContractRedeemer = Accept | Claim POSIXTime| End POSIXTime
    deriving Show
PlutusTx.unstableMakeIsData ''FContractRedeemer

-- Helper to go from a value in ada to lovelace
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-- Transition function which is the core business logic of this contract
-- TODO: Eliminate the curTime from determining the amount of funds available(unsecure)
{-# INLINABLE transition #-}
transition :: FreelanceContract -> State FContractDatum -> FContractRedeemer -> Maybe (TxConstraints Void Void, State FContractDatum)
transition fc s r =
    case (stateValue s, stateData s, r) of
        
        -- Accept must be before the deadline
        (v, FContractDatum _ valC, Accept)
             | lovelaces v == totalPayment fc           -> Just(
                                                                    Constraints.mustBeSignedBy (freelancer fc)        <>          
                                                                    Constraints.mustValidateIn (to $ signDeadline fc)
                                                                    , State (FContractDatum 0 0) (lovelaceValueOf $ totalPayment fc)
                                                                    )  
                          
        (v, FContractDatum _ valC, Claim curTime)

            -- No intervals complete
            -- Safe to use curtime here because no value is exchanged in this transition
            | curTime < (startTime fc + (POSIXTime $ intervalLength fc))              -> Nothing


            -- First interval complete
            -- Must validate from the end of the first interval to the beginning of the second
            | curTime <= startTime fc + (POSIXTime (intervalLength fc * 2))
            ,let finInt = 1                                -> Just(
                                                                        Constraints.mustBeSignedBy (freelancer fc)         <>                  
                                                                        Constraints.mustValidateIn (from $ firstInterval)                        <>
                                                                        Constraints.mustPayToPubKey (freelancer fc) (lovelaceValueOf $ claimableFunds finInt valC)
                                                                        , State (FContractDatum 1 (claimableFunds finInt valC))
                                                                        (lovelaceValueOf $ remainingFunds finInt valC)
                                                                        )

            -- Second interval complete                                                          
            | curTime <= startTime fc + (POSIXTime (intervalLength fc * 3))
            , let finInt = 2                               -> Just(
                                                                        Constraints.mustBeSignedBy (freelancer fc)         <>                  
                                                                        Constraints.mustValidateIn (from $ secondInterval)                        <>
                                                                        Constraints.mustPayToPubKey (freelancer fc) (lovelaceValueOf $ claimableFunds finInt valC)
                                                                        , State (FContractDatum 2 (valC + (claimableFunds finInt valC)))
                                                                        (lovelaceValueOf $ remainingFunds finInt valC)
                                                                        )  

            -- -- All three initervals finished so contract is finished
            | curTime > endTime
            , let finInt = 3                               ->  Just(
                                                                            Constraints.mustBeSignedBy (freelancer fc)                  <>
                                                                            Constraints.mustValidateIn (from $ thirdInterval)           <>
                                                                            Constraints.mustPayToPubKey (freelancer fc) (lovelaceValueOf $ claimableFunds finInt valC)
                                                                            , State Finished mempty
                                                                          )

        -- Does not matter who calls the End endpoint. Pay the freelancer the ada the have earned and
        -- the rest of the funds go to back to the employers wallet.
        -- NOTE: Grace period is hard coded at 8000ms(4 seconds/slots), it needs to be a parameter to the contract
        -- TODO: Constraints.mustSatisfyAnyOf [Constraints.mustBeSignedBy (freelancer fc), Constraints.mustBeSignedBy (empoyer fc)] is not yet exported/supported
        -- by Ledger.Constraints. Need them to add it in.
        -- TODO: Understand `hull` and why it validates
        
        (v, FContractDatum finInt valC, End curTime)
            | firstInterval < curTime               &&
              curTime <  (firstInterval + 8000) 
            , let finInt = 1                               ->  Just(
                                                                    Constraints.mustBeSignedBy (freelancer fc)                                                  <>
                                                                    Constraints.mustValidateIn (hull (from $ firstInterval)(to $ firstInterval + 8000))         <>
                                                                    Constraints.mustPayToPubKey (freelancer fc) (lovelaceValueOf $ claimableFunds finInt valC)  <>
                                                                    Constraints.mustPayToPubKey (employer fc) (lovelaceValueOf $ remainingFunds finInt valC)
                                                                    , State Finished mempty
            )

            | secondInterval < curTime              &&
              curTime <  (secondInterval + 8000)
            , let finInt = 2                                ->  Just(
                                                                    Constraints.mustBeSignedBy (freelancer fc)                                                  <>
                                                                    Constraints.mustValidateIn (hull (from $ secondInterval)(to $ secondInterval + 8000))         <>
                                                                    Constraints.mustPayToPubKey (freelancer fc) (lovelaceValueOf $ claimableFunds finInt valC)  <>
                                                                    Constraints.mustPayToPubKey (employer fc) (lovelaceValueOf $ remainingFunds finInt valC)
                                                                    , State Finished mempty
            )
            | thirdInterval < curTime              &&
              curTime <  (thirdInterval + 8000)
            , let finInt = 3                                ->  Just(
                                                                    Constraints.mustBeSignedBy (freelancer fc)                                                  <>
                                                                    Constraints.mustValidateIn (hull (from $ thirdInterval)(to $ thirdInterval + 8000))         <>
                                                                    Constraints.mustPayToPubKey (freelancer fc) (lovelaceValueOf $ claimableFunds finInt valC)  <>
                                                                    Constraints.mustPayToPubKey (employer fc) (lovelaceValueOf $ remainingFunds finInt valC)
                                                                    , State Finished mempty
            )
        -- _ -> Just (mempty , State Finished mempty)
        _                                        -> Nothing

    where 


        -- The total payment minus the amount earned by the freelancer + the value already claimed
        remainingFunds :: Integer -> Integer -> Integer
        remainingFunds finInt valC = totalPayment fc - ((claimableFunds finInt valC) + valC)

        -- Funds available to be claimed given the current time 
        claimableFunds :: Integer -> Integer -> Integer
        claimableFunds finInt valC = (finInt * intervalPay) - valC

        -- The amount paid per interval
        intervalPay :: Integer
        intervalPay = fst $ PlutusTx.Prelude.divMod (totalPayment fc) (numberOfIntervals fc)

        -- The lifetime of the contract in POSIXTime
        fullInterval :: POSIXTime
        fullInterval = POSIXTime $ intervalLength fc * numberOfIntervals fc

        -- The POSIXTime where the contract expires
        endTime :: POSIXTime
        endTime = (startTime fc) + fullInterval

        -- +1 is added to the start time to make sure startTime is evenly divisible by 1000
        firstInterval:: POSIXTime
        firstInterval = (startTime fc + 1) + POSIXTime(intervalLength fc)

        secondInterval :: POSIXTime
        secondInterval = (startTime fc +1) + POSIXTime(intervalLength fc * 2)

        thirdInterval :: POSIXTime
        thirdInterval = (startTime fc +1) + POSIXTime(intervalLength fc * 3)


{-# INLINABLE final #-}
final :: FContractDatum -> Bool
final Finished = True
final _        = False

{-# INLINABLE check #-}
check :: FContractDatum -> FContractRedeemer -> ScriptContext -> Bool
check _ _ _ = True

{-# INLINABLE fcontractStateMachine #-}
fcontractStateMachine :: FreelanceContract ->  StateMachine FContractDatum FContractRedeemer
fcontractStateMachine fcontract = StateMachine
    { smTransition  = transition fcontract
    , smFinal       = final
    , smCheck       = check
    , smThreadToken = cToken fcontract
    }

{-# INLINABLE mkFContractValidator #-}
mkFContractValidator :: FreelanceContract -> FContractDatum -> FContractRedeemer -> ScriptContext -> Bool
mkFContractValidator fcontract  = mkValidator $ fcontractStateMachine fcontract

type FContracting = StateMachine FContractDatum FContractRedeemer

fcontractStateMachine' :: FreelanceContract -> StateMachine FContractDatum FContractRedeemer
fcontractStateMachine' fcontract = fcontractStateMachine fcontract 

typedFContractValidator :: FreelanceContract -> Scripts.TypedValidator FContracting
typedFContractValidator fcontract = Scripts.mkTypedValidator @FContracting
    ($$(PlutusTx.compile [|| mkFContractValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode fcontract)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @FContractDatum @FContractRedeemer

fContractValidator :: FreelanceContract -> Validator
fContractValidator = Scripts.validatorScript . typedFContractValidator

fContractAddress :: FreelanceContract -> Ledger.Address
fContractAddress = scriptAddress . fContractValidator

fContractClient :: FreelanceContract -> StateMachineClient FContractDatum FContractRedeemer
fContractClient fcontract = mkStateMachineClient $ StateMachineInstance (fcontractStateMachine' fcontract) (typedFContractValidator fcontract)

-- Datatype outlining the params for the employer
data EmployerParams = EmployerParams
    { eFreelancer          :: !PubKeyHash,
      eNumberOfIntervals   :: !Integer,
      eIntervalLength      :: !Integer,
      eTotalPayment        :: !Integer,
      eStartTime           :: !POSIXTime,
      eSignDeadline        :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = void $ awaitTime t >> waitNSlots 1

-- Function for starting an instance of the freelancer contract
startFContract :: forall s. EmployerParams -> Contract (Last FreelanceContract) s Text ()
startFContract emp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    tt  <- Just <$> mapError' getThreadToken
    let fcontract   = FreelanceContract
            { employer          = pkh
            , freelancer        = eFreelancer emp
            , numberOfIntervals = eNumberOfIntervals emp
            , intervalLength    = eIntervalLength emp
            , startTime         = eStartTime emp
            , signDeadline      = eSignDeadline emp
            , totalPayment      = eTotalPayment emp
            , cToken            = tt
            }
        client = fContractClient fcontract
        v      = lovelaceValueOf (eTotalPayment emp)
    void $ mapError' $ runInitialise client (FContractDatum 0 0) v
    logInfo @String $ "A freelance contract has been created for this PKH: " ++ show (eFreelancer emp)
    tell $ Last $ Just fcontract

-- Datatype outlining the params for the freelancer
data FreelancerParams = FreelancerParams
    { fEmployer            :: !PubKeyHash,
      fNumberOfIntervals   :: !Integer,
      fIntervalLength      :: !Integer,
      fTotalPayment        :: !Integer,
      fStartTime           :: !POSIXTime,
      fSignDeadline        :: !POSIXTime,
      fToken               :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON)

-- Function for accepting a contract
acceptFContract :: FreelanceContract -> Contract w s Text ()
acceptFContract fp = do
    let client = fContractClient fp
    m <- mapError' $ getOnChainState client
    case m of
        Nothing             -> throwError "Contract ouput not found"
        Just ((o, _), _) -> case tyTxOutData o of
            FContractDatum _ _-> do
                logInfo @String "Contract found. Accepting..."
                void $ mapError' $ runStep client Accept
                logInfo @String "Contract Accepted"

            _ -> throwError "unexpected datum"

    -- Could add some automatic functionality for calling the claim endpoint
    -- waitUntilTimeHasPassed $ fIntervalLength fp * fNumberOfIntervals fp

-- Function for claiming funds from a contract
claimFContract :: FreelanceContract -> Contract w s Text ()
claimFContract fp = do
    now <- Contract.currentTime
    let client = fContractClient fp
    m <- mapError' $ getOnChainState client
    case m of
        Nothing             -> throwError "Contract ouput not found"
        Just ((o, _), _) -> case tyTxOutData o of

            FContractDatum _ _-> do
                logInfo @String "Contract Found... Claiming available funds"
                void $ mapError' $ runStep client $ Claim now
                logInfo @String "Funds Claimed."

            _ -> throwError "unexpected datum"

-- claimFFContract :: FreelanceContract -> Contract w s Text ()
-- claimFFContract fc= do
--     now <- Contract.currentTime
--     void $ mapError' $ runStep (fContractClient fc) $ Claim now


endFContract :: FreelanceContract -> Contract w s Text ()
endFContract fp = do
    now <- Contract.currentTime
    let client = fContractClient fp
    m <- mapError' $ getOnChainState client
    case m of
            Nothing             -> throwError "Contract ouput not found"
            Just ((o, _), _) -> case tyTxOutData o of

                FContractDatum _ _-> do
                    logInfo @String "Contract Found... Attempting to end"
                    void $ mapError' $ runStep client $ End now
                    logInfo @String "Contract Ended."

            _ -> throwError "unexpected datum"


type FStartSchema    = Endpoint "start"  ()
type FContractSchema = 
                       Endpoint "accept" ()
                   .\/ Endpoint "claim"  ()
                   .\/ Endpoint "end"    ()

startEndpoints :: EmployerParams -> Contract (Last FreelanceContract) FStartSchema Text ()
startEndpoints ep = h $ do 
    endpoint @"start" 
    startFContract ep
    where
        -- Error handling function to log the error
        h :: Contract (Last FreelanceContract) FStartSchema Text () -> Contract (Last FreelanceContract) FStartSchema Text ()
        h = handleError logError

--
endpoints :: FreelanceContract -> Contract () FContractSchema Text ()
endpoints fc = (accept `select` claim `select` end) >> endpoints fc
  where
    accept :: Contract () FContractSchema Text ()
    accept = h $ do
        endpoint @"accept" 
        acceptFContract fc

    claim :: Contract () FContractSchema Text ()
    claim = h $ do
        endpoint @"claim"
        claimFContract fc

    end :: Contract () FContractSchema Text ()
    end = h $ do
        endpoint @"end"
        endFContract fc

    -- Error handling function to log the error
    h :: Contract () FContractSchema Text () -> Contract () FContractSchema Text ()
    h = handleError logError