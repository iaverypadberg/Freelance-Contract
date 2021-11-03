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

module UnitTests.BasicTests (tests) where

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

-- NOTEWORTHY: slotToEndPOSIXTime def 10 gives a number 1 less than what is needed to successfully convert a POSIXTime to a slot.
-- need to do a +1 on the on chain code.

-- Test Group Characteristics: 
-- 1. Contract allways runs through its full course(no 'end' endpoint)
-- 2. The full amount of funds will always be transferred

-- Test Cases Covered - Basic Params
-- Test 1 : Accept on time, claim during first interval, claim during second inteval, claim during third interval, claim after contract end
-- Test 2 : Accept on time, claim during first interval, second interval, claim after contract end
-- Test 3 : Accept on time, claim during first interval, claim during third interval, claim after contract end
-- Test 4 : Accept on time, claim during first interval, claim after contract end
-- Test 5 : Accept on time, claim during second interval, claim during third interval, claim after contract
-- Test 6 : Accept on time, claim during second interval, claim after contract end
-- Test 7 : Accept on time, claim during third interval, claim after contract end
-- Test 8 : Accept on time, claim after contract end



-- tests :: IO ()
-- tests = defaultMain testThese

tests :: TestTree
tests = testGroup "Tests" [test1 empParams, test2 empParams, test3 empParams,test4 empParams,test5 empParams,test6 empParams,test7 empParams,test8 empParams]

-- Basic Params
empParams :: EmployerParams
empParams = EmployerParams{
                  eFreelancer           = pubKeyHash $ walletPubKey $ Wallet 2
                , eNumberOfIntervals    = 3
                , eIntervalLength       = 10000
                , eTotalPayment         = 9_000_000
                , eStartTime            = slotToEndPOSIXTime def 20
                , eSignDeadline         = slotToEndPOSIXTime def 10
                }

-- #############################  Test 1  #############################
test1 :: EmployerParams -> TestTree
test1 p = checkPredicate
    "test1"
    (     assertNoFailedTransactions
     .&&. walletFundsChange (Wallet 1) (Ada.lovelaceValueOf  (-9_000_000))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf   9_000_000)
    )
    $ trace1 p
        

trace1 :: EmployerParams -> EmulatorTrace ()
trace1 ep = do
    -- Wont change
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints
    -- Wont change
    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
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

    -- Extras.logInfo $ "StartTime: " ++ show (fStartTime fp) ++ "Slot 13 Time:" ++ show (slotToEndPOSIXTime def 13)

    -- This is where the traces will differ
    void $ Emulator.waitNSlots 2 

    callEndpoint @"accept" h2 fp    -- slot 4

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    callEndpoint @"claim" h2 fp     -- first interval

    void $ Emulator.waitNSlots 12   -- slot 32

    callEndpoint @"claim" h2 fp     -- second interval     

    void $ Emulator.waitNSlots 10   -- slot 42

    callEndpoint @"claim" h2 fp     -- third interval

    void $ Emulator.waitNSlots 10   -- slot 52

    callEndpoint @"claim" h2 fp     -- after contract end
    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt

-- #############################  Test 2  #############################
test2 :: EmployerParams -> TestTree
test2 p = checkPredicate
    "test2"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf  (-9_000_000))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf   9_000_000)
    )
    $ trace2 p



trace2 :: EmployerParams -> EmulatorTrace ()
trace2 ep = do

       -- Wont change
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    -- Wont change
    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1

    
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

    -- Call accept after the time has passed
    void $ Emulator.waitNSlots 2 

    callEndpoint @"accept" h2 fp    -- slot 4

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    callEndpoint @"claim" h2 fp     -- first interval

    void $ Emulator.waitNSlots 12   -- slot 32

    callEndpoint @"claim" h2 fp     -- second interval     

    void $ Emulator.waitNSlots 10   -- slot 42

    void $ Emulator.waitNSlots 10   -- slot 52

    callEndpoint @"claim" h2 fp     -- after contract end
    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt


-- #############################  Test 3  #############################
test3 :: EmployerParams -> TestTree
test3 p = checkPredicate
    "test3"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf  (-9_000_000))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf   9_000_000)
    )
    $ trace3 p



trace3 :: EmployerParams -> EmulatorTrace ()
trace3 ep = do

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

    -- Call accept after the time has passed
    void $ Emulator.waitNSlots 2 

    callEndpoint @"accept" h2 fp    -- slot 4

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    callEndpoint @"claim" h2 fp     -- first interval 

    void $ Emulator.waitNSlots 12   -- slot 32   

    void $ Emulator.waitNSlots 10   -- slot 42

    callEndpoint @"claim" h2 fp     -- third interval 

    void $ Emulator.waitNSlots 10   -- slot 52

    callEndpoint @"claim" h2 fp     -- after contract end
    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt

-- #############################  Test 4  #############################
test4 :: EmployerParams -> TestTree
test4 p = checkPredicate
    "test4"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf  (-9_000_000))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf   9_000_000)
    )
    $ trace4 p



trace4 :: EmployerParams -> EmulatorTrace ()
trace4 ep = do

       -- Wont change
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    -- Wont change
    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
    
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

    -- Call accept after the time has passed
    void $ Emulator.waitNSlots 2 

    callEndpoint @"accept" h2 fp    -- slot 4

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    callEndpoint @"claim" h2 fp     -- first interval  

    void $ Emulator.waitNSlots 12   -- slot 32 

    void $ Emulator.waitNSlots 10   -- slot 42

    void $ Emulator.waitNSlots 10   -- slot 52

    callEndpoint @"claim" h2 fp     -- after contract end
    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt

-- #############################  Test 5  #############################
test5 :: EmployerParams -> TestTree
test5 p = checkPredicate
    "test5"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf  (-9_000_000))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf   9_000_000)
    )
    $ trace5 p



trace5 :: EmployerParams -> EmulatorTrace ()
trace5 ep = do

       -- Wont change
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    -- Wont change
    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
    
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

    -- Call accept after the time has passed
    void $ Emulator.waitNSlots 2 

    callEndpoint @"accept" h2 fp    -- slot 4

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    void $ Emulator.waitNSlots 12   -- slot 32

    callEndpoint @"claim" h2 fp     -- second interval

    void $ Emulator.waitNSlots 10   -- slot 42

    callEndpoint @"claim" h2 fp     -- third interval  

    void $ Emulator.waitNSlots 10   -- slot 52

    callEndpoint @"claim" h2 fp     -- after contract end
    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt


-- #############################  Test 6  #############################
test6 :: EmployerParams -> TestTree
test6 p = checkPredicate
    "test6"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf  (-9_000_000))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf   9_000_000)
    )
    $ trace6 p



trace6 :: EmployerParams -> EmulatorTrace ()
trace6 ep = do

       -- Wont change
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    -- Wont change
    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
    
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

    -- Call accept after the time has passed
    void $ Emulator.waitNSlots 2 

    callEndpoint @"accept" h2 fp    -- slot 4

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    void $ Emulator.waitNSlots 12   -- slot 32 

    callEndpoint @"claim" h2 fp     -- second interval

    void $ Emulator.waitNSlots 10   -- slot 42

    void $ Emulator.waitNSlots 10   -- slot 52

    callEndpoint @"claim" h2 fp     -- after contract end
    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt

-- #############################  Test 7  #############################
test7 :: EmployerParams -> TestTree
test7 p = checkPredicate
    "test7"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf  (-9_000_000))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf   9_000_000)
    )
    $ trace7 p



trace7 :: EmployerParams -> EmulatorTrace ()
trace7 ep = do

       -- Wont change
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    -- Wont change
    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
    
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

    -- Call accept after the time has passed
    void $ Emulator.waitNSlots 2 

    callEndpoint @"accept" h2 fp    -- slot 4

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    void $ Emulator.waitNSlots 12   -- slot 32 

    void $ Emulator.waitNSlots 10   -- slot 42

    callEndpoint @"claim" h2 fp     -- third interval  

    void $ Emulator.waitNSlots 10   -- slot 52

    callEndpoint @"claim" h2 fp     -- after contract end
    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt


-- #############################  Test 8  #############################
test8 :: EmployerParams -> TestTree
test8 p = checkPredicate
    "test8"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf  (-9_000_000))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf   9_000_000)
    )
    $ trace8 p



trace8 :: EmployerParams -> EmulatorTrace ()
trace8 ep = do

       -- Wont change
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    -- Wont change
    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
    
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

    -- Call accept after the time has passed
    void $ Emulator.waitNSlots 2 

    callEndpoint @"accept" h2 fp    -- slot 4

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    void $ Emulator.waitNSlots 12   -- slot 32 

    void $ Emulator.waitNSlots 10   -- slot 42

    void $ Emulator.waitNSlots 10   -- slot 52

    callEndpoint @"claim" h2 fp     -- after contract end
    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt


-- #############################  Test 9  #############################
test9 :: EmployerParams -> TestTree
test9 p = checkPredicate
    "test9"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf  (-9_000_000))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf   9_000_000)
    )
    $ trace9 p



trace9 :: EmployerParams -> EmulatorTrace ()
trace9 ep = do

       -- Wont change
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    -- Wont change
    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
    
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

    -- Call accept after the time has passed
    void $ Emulator.waitNSlots 2 

    callEndpoint @"accept" h2 fp    -- slot 4

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    void $ Emulator.waitNSlots 12   -- slot 32 is second interval

    void $ Emulator.waitNSlots 10   -- slot 42

    callEndpoint @"claim" h2 fp     -- third interval  

    void $ Emulator.waitNSlots 10   -- slot 52

    callEndpoint @"claim" h2 fp     -- after contract end
    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt

-- #############################  Test 10  #############################
test10 :: EmployerParams -> TestTree
test10 p = checkPredicate
    "test4"
    (     walletFundsChange (Wallet 1) (Ada.lovelaceValueOf  (-9_000_000))
     .&&. walletFundsChange (Wallet 2) (Ada.lovelaceValueOf   9_000_000)
    )
    $ trace10 p



trace10 :: EmployerParams -> EmulatorTrace ()
trace10 ep = do

       -- Wont change
    h1 <- activateContractWallet (Wallet 1) endpoints
    h2 <- activateContractWallet (Wallet 2) endpoints

    -- Wont change
    let pkh1      = pubKeyHash $ walletPubKey $ Wallet 1
    
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

    -- Call accept after the time has passed
    void $ Emulator.waitNSlots 2 

    callEndpoint @"accept" h2 fp    -- slot 4

    void $ Emulator.waitNSlots 16   -- slot 20 is start

    void $ Emulator.waitNSlots 12   -- slot 32 is second interval

    void $ Emulator.waitNSlots 10   -- slot 42

    callEndpoint @"claim" h2 fp     -- third interval  

    void $ Emulator.waitNSlots 10   -- slot 52

    callEndpoint @"claim" h2 fp     -- after contract end
    
  where
    getTT :: ContractHandle (Last ThreadToken) FContractSchema Text -> EmulatorTrace ThreadToken
    getTT h = do
        void $ Emulator.waitNSlots 1
        Last m <- observableState h
        case m of
            Nothing -> getTT h
            Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt






