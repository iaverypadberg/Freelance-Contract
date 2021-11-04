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

    h1 <- activateContractWallet (Wallet 1) $ startEndpoints ep 

    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
        pkh2      = pubKeyHash $ walletPubKey $ Wallet 2

    callEndpoint @"start" h1 ()
    void $ Emulator.waitNSlots 10

    Last m <- observableState h1
    void $ Emulator.waitNSlots 5
    -- case m of
    --     Nothing -> Extras.logError @String "error starting token sale"
    --     Just fc -> do
            
    --         Extras.logInfo $ "Freelance contract started" ++ show fc

    --         h1 <- activateContractWallet (Wallet 1) $ endpoints fc 
    --         h2 <- activateContractWallet (Wallet 2) $ endpoints fc
    --         h3 <- activateContractWallet (Wallet 3) $ endpoints fc

    --         void $ Emulator.waitNSlots 2

    --         callEndpoint @"accept" h2 ()

    --         -- Call accept after the time has passed

    --         void $ Emulator.waitNSlots 16   -- slot 20 is start

    --         void $ Emulator.waitNSlots 15   -- slot 35 is second 
            
    --         callEndpoint @"claim" h2 ()

    --         void $ Emulator.waitNSlots 6   -- slot 45

    --         callEndpoint @"end" h2 ()     -- third interval  

    --         void $ Emulator.waitNSlots 10   -- slot 55

    --         callEndpoint @"claim" h2 ()

    
--   where
--     getTT :: ContractHandle (Last ThreadToken) fcontractSchema Text -> EmulatorTrace ThreadToken
--     getTT h = do
--         void $ Emulator.waitNSlots 1
--         Last m <- observableState h
--         case m of
--             Nothing -> getTT h
--             Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt