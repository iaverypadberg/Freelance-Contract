{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Contract.FreelanceTrace (testTrace) where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import           Ledger
import           Ledger.Ada                 as Ada
import           Ledger.TimeSlot
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..), String, mod)
import           Wallet.Emulator.Wallet
import           Plutus.Contract.Test
import           Test.Tasty

import           Contract.Freelance

-- Trace for testing out functionality
testTrace :: IO ()
testTrace = runEmulatorTraceIO $ traceIt empParams
    where
        empParams :: EmployerParams
        empParams = EmployerParams{
                  eFreelancer           = pubKeyHash $ walletPubKey $ Wallet 2
                , eNumberOfIntervals    = 3
                , eIntervalLength       = 10000
                , eTotalPayment         = 9_000_000
                -- 1596059101999
                -- 1596059101999
                , eStartTime            = slotToEndPOSIXTime def 20
                , eSignDeadline         = slotToEndPOSIXTime def 10
                }

traceIt :: EmployerParams -> EmulatorTrace ()
traceIt ep = do
    -- Wont change
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    -- Wont change
    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
        pkh2      = pubKeyHash $ walletPubKey $ Wallet 2
    -- Wont change
    callEndpoint @"start" h1 ep
    -- Wont change
    tt <- getTT h1
    let fp = FreelancerParams
                { fEmployer             = pkh1
                , fNumberOfIntervals    = eNumberOfIntervals ep
                , fIntervalLength       = eIntervalLength ep
                , fTotalPayment         = eTotalPayment ep
                , fStartTime            = eStartTime ep
                , fSignDeadline         = eSignDeadline ep
                , fToken                = tt
                }
    -- This is where the traces will differ
    void $ Emulator.waitNSlots 2

    callEndpoint @"accept" h2 fp

    Extras.logInfo $ "StartTime: " ++ show (fStartTime fp) ++ "Slot 13 Time:" ++ show (slotToEndPOSIXTime def 13)
    -- Call accept after the time has passed

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    void $ Emulator.waitNSlots 15   -- slot 35 is second 
    
    callEndpoint @"claim" h2 fp

    void $ Emulator.waitNSlots 10   -- slot 45

    callEndpoint @"end" h2 fp     -- third interval  

    void $ Emulator.waitNSlots 10   -- slot 55

    callEndpoint @"claim" h2 fp

    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt