{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PropertyTests.Model
(
    -- ( tests
    -- , test
    -- , TSModel (..)
    )  where

import           Control.Lens                       hiding (elements)
import           Control.Monad                      (void, when)
import           Data.Default                       (Default (..))
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Maybe                         (isJust, isNothing)
import           Data.Monoid                        (Last (..))
import           Data.String                        (IsString (..))
import           Data.Text                          (Text)
import           Plutus.Contract.Test
import           Plutus.Contract.Test.ContractModel
import           Plutus.Trace.Emulator
import           Ledger                             hiding (singleton)
import           Ledger.Ada                         as Ada
import           Ledger.Value
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

-- import           NFTContract.NFT                   (ManageNFT (..),CreatorParams (..), BuyerParams (..), NFTStartSchema, NFTUseSchema, Last (..), ThreadToken, Text, startEndpoint, useEndpoints)

-- -- NFT Sale State =
-- data NFTState = NFTSState
--     { _nftsPrice    :: !Integer
--     , _nftsLovelace :: !Integer
--     , _nftsToken    :: !Integer
--     } deriving Show

-- makeLenses ''NFTSState

-- newtype NFTSModel = NFTSModel {_nftsModel :: Map Wallet NFTSState}
--     deriving Show

-- makeLenses ''NFTSModel

-- tests :: TestTree
-- tests = testProperty "token sale model" prop_TS

-- instance ContractModel NFTSModel where

-- -- First wallet is always the owner of the NFTSale
--     data Action NFTSModel =
--               Start Wallet
--             | List Wallet Wallet Integer
--             | Buy Wallet Wallet
--             | Remove Wallet Wallet
--         deriving (Show, Eq)

--     data ContractInstanceKey NFTSModel w s e where
--         StartKey :: Wallet           -> ContractInstanceKey NFTSModel (Last TokenSale) NFTStartSchema Text
--         UseKey   :: Wallet -> Wallet -> ContractInstanceKey NFTSModel ()               NFTUseSchema   Text

--     instanceTag key _ = fromString $ "instance tag for: " ++ show key

--     arbitraryAction _ = oneof $
--         (Start <$> genWallet) :
--         [ List  <$> genWallet <*> genWallet <*> genNonNeg ]     ++
--         [ Buy <$> genWallet <*> genWallet <*> genNonNeg ]       ++
--         [ Remove <$> genWallet <*> genWallet <*> genNonNeg ]

--     initialState = NFTSModel Map.empty

--     nextState (Start w) = do
--         (nftsModel . at w) $= Just (NFTSState 0 0 0)
--         wait 1

--     nextState (List v w p) = do
--         started <- hasStarted v 
--         when (v == w && starter && p > 0) $ do
--             (nftsModel . ix v . nftsPrice) $= p
--             bc <- askModelState $ view $ balanceChange w
--             let token = tokens Map.! v
--             -- The below line is confusing
--             when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
--                 withdraw w $ assetClassValue token n
--                 (tsModel . ix v . tssToken) $~ (+ n)
--         wait 1

--     nextState (Remove v w n) = do
--         started <- hasStarted v                                     -- has the token sale started?
--         when (n > 0 && started) $ do
--             bc <- askModelState $ view $ balanceChange w
--             let token = tokens Map.! v
--             when (tokenAmt + assetClassValueOf bc token >= n) $ do  -- does the wallet have the tokens to give?
--                 withdraw w $ assetClassValue token n
--                 (tsModel . ix v . tssToken) $~ (+ n)
--         wait 1

--     nextState (Buy v w n) = do
--         when (n > 0) $ do
--             m <- getTSState v
--             case m of
--                 Just t
--                     | t ^. tssToken >= n -> do
--                         let p = t ^. tssPrice
--                             l = p * n
--                         withdraw w $ lovelaceValueOf l
--                         deposit w $ assetClassValue (tokens Map.! v) n
--                         (tsModel . ix v . tssLovelace) $~ (+ l)
--                         (tsModel . ix v . tssToken)    $~ (+ (- n))
--                 _ -> return ()
--         wait 1

--     nextState (Withdraw v w n l) = do
--         when (v == w) $ do
--             m <- getTSState v
--             case m of
--                 Just t
--                     | t ^. tssToken >= n && t ^. tssLovelace >= l -> do
--                         deposit w $ lovelaceValueOf l <> assetClassValue (tokens Map.! w) n
--                         (tsModel . ix v . tssLovelace) $~ (+ (- l))
--                         (tsModel . ix v . tssToken) $~ (+ (- n))
--                 _ -> return ()
--         wait 1

--     perform h _ cmd = case cmd of
--         (Start w)          -> callEndpoint @"start"      (h $ StartKey w) (tokenCurrencies Map.! w, tokenNames Map.! w, False) >> delay 1
--         (SetPrice v w p)   -> callEndpoint @"set price"  (h $ UseKey v w) p                                                    >> delay 1
--         (AddTokens v w n)  -> callEndpoint @"add tokens" (h $ UseKey v w) n                                                    >> delay 1
--         (BuyTokens v w n)  -> callEndpoint @"buy tokens" (h $ UseKey v w) n                                                    >> delay 1
--         (Withdraw v w n l) -> callEndpoint @"withdraw"   (h $ UseKey v w) (n, l)                                               >> delay 1

--     precondition s (Start w)          = isNothing $ getTSState' s w
--     precondition s (SetPrice v _ _)   = isJust    $ getTSState' s v
--     precondition s (AddTokens v _ _)  = isJust    $ getTSState' s v
--     precondition s (BuyTokens v _ _)  = isJust    $ getTSState' s v
--     precondition s (Withdraw v _ _ _) = isJust    $ getTSState' s v

-- deriving instance Eq (ContractInstanceKey TSModel w s e)
-- deriving instance Show (ContractInstanceKey TSModel w s e)

-- getTSState' :: ModelState NFTSModel -> Wallet -> Maybe NFTSState
-- getTSState' s v = s ^. contractState . nftsModel . at v

-- getTSState :: Wallet -> Spec NFTSModel (Maybe NFTSState)
-- getTSState v = do
--     s <- getModelState
--     return $ getTSState' s v

-- hasStarted :: Wallet -> Spec NFTSModel Bool
-- hasStarted v = isJust <$> getTSState v

-- w1, w2 :: Wallet
-- w1 = Wallet 1
-- w2 = Wallet 2

-- wallets :: [Wallet]
-- wallets = [w1, w2]

-- tokenCurrencies :: Map Wallet CurrencySymbol
-- tokenCurrencies = Map.fromList $ zip wallets ["aa", "bb"]

-- tokenNames :: Map Wallet TokenName
-- tokenNames = Map.fromList $ zip wallets ["A", "B"]

-- tokens :: Map Wallet AssetClass
-- tokens = Map.fromList [(w, AssetClass (tokenCurrencies Map.! w, tokenNames Map.! w)) | w <- wallets]

-- tss :: Map Wallet TokenSale
-- tss = Map.fromList
--     [ (w, TokenSale { tsSeller = pubKeyHash $ walletPubKey w
--                     , tsToken  = tokens Map.! w
--                     , tsTT     = Nothing
--                     })
--     | w <- wallets
--     ]

-- delay :: Int -> EmulatorTrace ()
-- delay = void . waitNSlots . fromIntegral

-- instanceSpec :: [ContractInstanceSpec TSModel]
-- instanceSpec =
--     [ContractInstanceSpec (StartKey w) w startEndpoint | w <- wallets] ++
--     [ContractInstanceSpec (UseKey v w) w $ useEndpoints $ tss Map.! v | v <- wallets, w <- wallets]

-- genWallet :: Gen Wallet
-- genWallet = elements wallets

-- genNonNeg :: Gen Integer
-- genNonNeg = getNonNegative <$> arbitrary

-- tokenAmt :: Integer
-- tokenAmt = 1_000

-- prop_TS :: Actions TSModel -> Property
-- prop_TS = withMaxSuccess 100 . propRunActionsWithOptions
--     (defaultCheckOptions & emulatorConfig .~ EmulatorConfig (Left d) def def)
--     instanceSpec
--     (const $ pure True)
--   where
--     d :: InitialDistribution
--     d = Map.fromList $ [ ( w
--                          , lovelaceValueOf 1_000_000_000 <>
--                            mconcat [assetClassValue t tokenAmt | t <- Map.elems tokens])
--                        | w <- wallets
--                        ]

-- test :: IO ()
-- test = quickCheck prop_TS
