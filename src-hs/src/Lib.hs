{-# LANGUAGE ExplicitForAll #-}
-- https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Implementation
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

-- import Prelude
import Prelude hiding ((.), log, (!!))
import Control.Category ((<<<), (>>>))
-- import Data.Array (Array(..))
-- import Data.Array (index, snoc, nub, mapMaybe)
import Data.Traversable (mapM)
-- import Data.Either (Either(..), either, hush)
import Data.Either (rights, lefts)
import Data.Either.Combinators (rightToMaybe)

import Data.Foldable (foldl)
-- import qualified Data.Function as Function
-- import qualified Data.Int (fromString)
-- import Data.Map (Map)
-- import Data.Map as Map
import Data.HashMap.Lazy (HashMap, insert, lookup, elems)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (Maybe(..), fromMaybe, maybe)
-- import qualified Data.Number (fromString)
-- import Data.String (Pattern(..))
-- import Data.String.Common (split)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Tuple ( fst)
-- import Data.Tuple.Nested (Tuple3, (/\))
-- import IO (Effect, foreachE)
-- import Effect.Console (log)
-- import Node.Encoding (Encoding(..))
-- import Node.FS.Sync (readTextFile)

-- import Control.Lens -- (Cons, Snoc, Ixed, Index, IxValue, (^?), ix, _head, _tail, _last, _init)
import Control.Lens hiding ((<|), (|>))
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Maybe(Maybe)

import System.IO

-- import Time (now)
import Data.Time.Clock (getCurrentTime)

import Data


-- (<|) = ($)
-- infixr 0 <|
-- (|>) = flip ($)
-- infixl 0 |>

--helps with dot notation
-- https://ghc.haskell.org/trac/ghc/ticket/14812
-- (.) = flip ($)

-- concatMap :: forall a b. (a -> [b]) -> [a] -> [b]
-- concatMap = flip bind

singleton :: forall a. a -> [a]
singleton a = [a]

mapMaybe :: forall a b. (a -> Maybe b) -> [a] -> [b]
mapMaybe f = concatMap (maybe [] singleton <<< f)

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

-- https://github.com/qfpl/papa/blob/536b0a9243802347c299e077b5d85beb80d3a4a1/papa-lens-implement/src/Papa/Lens/Implement/Data/List.hs
(!!) ::
  Ixed s =>
  s
  -> Index s
  -> Maybe (IxValue s)
q !! n =
  q ^? ix n

infixl 9 !!

log :: String -> IO ()
log = putStrLn

-- |  To run:
-- |  $ node -e "require('./output/Main').main()"
main :: IO ()
main = do
  log ""

--   tt0 <- getCurrentTime
--   accountsText <- readFile "accounts-1m.txt" 
--   tt1 <- getCurrentTime
--   log <| "Read Accounts Complete " <> (show <| tt1 - tt0)
--   transactionsText <- readFile "transactions-1m.txt"
--   tt2 <- getCurrentTime
--   log <| "Read Transactions Complete " <> (show <| tt2 - tt1)

--   let accounts = parseAccounts accountsText
--   tt3 <- getCurrentTime
--   log <| "Parse Accounts complete " <> (show <| tt3 - tt2)
  
--   let validAccounts = rights accounts
--   tt4 <- getCurrentTime
--   log <| "Accounts Right Complete " <> (show <| tt4 - tt3)
  
--   let accountLookup = createAccountLookup validAccounts
--   tt5 <- getCurrentTime
--   log <| "Create Account Lookup complete " <> (show <| tt5 - tt4)
  
--   let transactions = parseTransactions transactionsText
--   let validTransactions = rights transactions
--   tt6 <- getCurrentTime
--   log <| "Parse Transaction Complete " <> (show <| tt6 - tt5)
  
--   let (errors, accountMap) = processTransactions accountLookup validTransactions
--   tt7 <- getCurrentTime
--   log <| "Process Transactions Complete " <> (show <| tt7 - tt6)

--   let processedValues = elems accountMap
--   tt8 <- getCurrentTime
--   log <| "Values ToArray Complete " <> (show <| tt8 - tt7)

--   traverse ( show >>> log ) processedValues
--   tt9 <- getCurrentTime

--   log <| "Read Accounts Complete " <> (show <| tt1 - tt0)
--   log <| "Read Transactions Complete " <> (show <| tt2 - tt1)
--   log <| "Parse Accounts complete " <> (show <| tt3 - tt2)
--   log <| "Accounts Right Complete " <> (show <| tt4 - tt3)
--   log <| "Create Account Lookup complete " <> (show <| tt5 - tt4)
--   log <| "Parse Transaction Complete " <> (show <| tt6 - tt5)
--   log <| "Process Transactions Complete " <> (show <| tt7 - tt6)
--   log <| "Values ToArray Complete " <> (show <| tt8 - tt7)
--   log <| "Complete " <> (show <| tt9 - tt8)
--   log <| "Total " <> (show <| tt9 - tt0)
--   pure ()


-- -- data Currency = USD | MXN | EUD | THB | GBP
-- --     deriving (Eq, Ord)
-- -- instance Show Currency where
-- --   show USD = "USD"
-- --   show MXN = "MXN"
-- --   show EUD = "EUD"
-- --   show THB = "THB"
-- --   show GBP = "GBP"

-- -- data Money = Money
-- --   { value :: Int
-- --   , currency :: Currency
-- --   }

-- convert :: HashMap Currency (HashMap Currency Int) -> Money -> Currency -> Maybe Money
-- convert conversionRates money targetCurrency =
--   if currency money == targetCurrency
--   then Just <| money
--   else do
--     currencySpecificConversionRates <- HM.lookup (money.currency) conversionRates
--     conversionRate <- HM.lookup targetCurrency currencySpecificConversionRates
--     Just <| Money { value = money.value * conversionRate, currency= targetCurrency }

-- -- newtype AccountNumber = AccountNumber String

-- -- data Account = Account
-- --   { _accountNumber :: AccountNumber
-- --   , _balance :: Money
-- --   , _name :: String
-- --   }
-- -- makeLenses ''Account

-- -- data Transaction
-- --   = Bill 
-- --     { _accountNumber :: AccountNumber
-- --     , _amount :: Money
-- --     , _bucket :: String
-- --     }
-- --   | Payment
-- --     { _accountNumber :: AccountNumber
-- --     , _amount :: Money
-- --     , _source :: String
-- --     }
-- -- makePrisms ''Transaction


-- -- When I try to run this theres a parse error. Any ideas? Do I need to do something special to turn on template haskell?
-- -- import Control.Lens.TH (makeLenses, makePrisms)
-- -- data Foo = Foo { a :: string }
-- -- makeLenses ''Foo
-- -- "Parse error: module header, import declaration or top-level declaration expected."


-- instance Show Transaction where
--   show (Bill {accountNumber, amount, bucket}) = 
--     "Bill - accountNumber: " 
--       <> (show accountNumber) 
--       <> ", amount: "
--       <> (show amount)
--       <> ", bucket: "
--       <> bucket
--   show (Payment {accountNumber, amount, source}) = 
--     "Bill - accountNumber: " 
--       <> (show accountNumber) 
--       <> ", amount: "
--       <> (show amount)
--       <> ", source: "
--       <> source

-- -- rights :: forall a b. [Either a b] -> [b]
-- -- rights array = mapMaybe hush array

-- -- lefts :: forall a b. [Either a b] -> [a]
-- -- lefts array = mapMaybe (either Just (const Nothing)) array

-- conversionRates :: [(Currency, Currency, Int)]
-- conversionRates =
--     [ (USD, MXN, 1.5)
--     , (USD, EUD, 2.5) 
--     , (USD, THB, 3.5) 
--     , (USD, GBP, 4.5) 
--     , (MXN, EUD, 2.5) 
--     , (MXN, THB, 3.5) 
--     , (MXN, GBP, 4.5) 
--     , (EUD, THB, 3.5) 
--     , (EUD, GBP, 4.5) 
--     , (THB, GBP, 4.5) 
--     ]

-- currencyConversionLookup :: HashMap Currency (HashMap Currency Int)
-- currencyConversionLookup = makeCurrencyConversionLookup conversionRates

-- makeCurrencyConversionLookup :: [(Currency, Currency, Int)] -> HashMap Currency (HashMap Currency Int)
-- makeCurrencyConversionLookup currencyMappings =
--   let
--     currencies :: [Currency]
--     currencies = nub <| Prelude.map fst currencyMappings <> map (\(a, b, c) -> b) currencyMappings
    
--     currencyLookup :: HashMap Currency (HashMap Currency Int)
--     currencyLookup = foldl (\accumulator currency -> HM.insert currency HM.empty accumulator) HM.empty currencies
    
--     insertRateAndInverse :: HashMap Currency (HashMap Currency Int) -> (Currency, Currency, Int) -> Maybe (HashMap Currency (HashMap Currency Int))
--     insertRateAndInverse currencyMap (from, to, rate) = do  
--       fromCurrencyMap <- HM.lookup from currencyMap
--       toCurrencyMap <- HM.lookup to currencyMap
--       let newFromMap = HM.insert to rate fromCurrencyMap :: HashMap Currency Int
--       let newToMap = HM.insert from (1.0/rate) toCurrencyMap

--       HM.insert from newFromMap currencyMap
--         |> HM.insert to newToMap 
--         |> pure
--   in
--     foldl 
--     (\allMappings conversionRate -> 
--         fromMaybe allMappings (insertRateAndInverse allMappings conversionRate)
--     ) 
--     currencyLookup 
--     currencyMappings

-- account = Account { accountNumber = AccountNumber "123", balance = Money { value = 100.0, currency = USD}, name = "John Doe"}
-- payment = Payment { accountNumber = AccountNumber "123", amount = Money { value = 50.0, currency = USD}, source = "Online Payment"}
-- bill = Bill { accountNumber = AccountNumber "123", amount = Money { value = 25.0, currency = USD}, bucket = "Dues"}

-- accountMap = createAccountLookup [account]
-- transactionList = [payment, bill, payment]
      

-- parseAccounts :: String -> [Either String Account]
-- parseAccounts text =
--   let
--     lines = splitOn "\n" text
--     lineOrErrorMessage line = 
--       maybe 
--         (Left <| "Error parsing account line: " <> line) 
--         Right
--         (parseAccount line)
--   in
--     Prelude.map lineOrErrorMessage lines

-- parseAccount :: String -> Maybe Account
-- parseAccount text = do
--   let list = splitOn "|" text
--   a <- list !! 0
--   b <- list !! 1
--   name <- list !! 2
--   accountNumber <- getAccountNumber a
--   balance <- getAmount b
--   pure Account { accountNumber, balance, name }
  
-- getAccountNumber :: String -> Maybe AccountNumber
-- getAccountNumber s = do
--   num <- readMaybe s :: Maybe Int
--   pure (AccountNumber (show num))
--     -- >>= (show >>> pure)
--   -- this is the same thing using do notation instead of bind (aka >>=) directly
--   -- do
--   --   accountText <- index fields i
--   --   accountNumber <- DataInt.fromString accountText
--   --   pure <| show accountNumber

-- getAmount :: String -> Maybe Money
-- getAmount amountText = do
--   let amountParts = splitOn " " amountText
--   firstText <- amountParts !! 0
--   currencyText <- amountParts !! 1
--   value <- readMaybe firstText :: Maybe Int
--   currency <- parseCurrency currencyText
--   pure (Money {value, currency})

-- parseCurrency :: String -> Maybe Currency
-- parseCurrency "USD" = Just USD
-- parseCurrency "MXN" = Just MXN
-- parseCurrency "GBP" = Just GBP
-- parseCurrency "EUD" = Just EUD
-- parseCurrency "THB" = Just THB
-- parseCurrency _ = Nothing

-- parseTransactions :: String -> [Either String Transaction]
-- parseTransactions text =
--   let
--     lines = splitOn "\n" text
--     lineOrErrorMessage line = 
--       maybe 
--         (Left <| "Error parsing transaction line: " <> line)
--         Right
--         (parseTransaction line)
--   in
--     Prelude.map lineOrErrorMessage lines

-- parseTransaction :: String -> Maybe Transaction
-- parseTransaction text = do
--   let fields = splitOn "|" text
--   a <- fields !! 0
--   b <- fields !! 1
--   accountNumber <- getAccountNumber a
--   amount <- getAmount b
--   transtype <- fields !! 2
--   transDetails <- fields !! 3
--   case transtype of
--     "Bill" -> pure <| Bill { accountNumber, amount, bucket = transDetails}
--     "Payment" -> pure <| Payment { accountNumber, amount, source = transDetails}
--     _ -> Nothing

-- createAccountLookup :: [Account] -> HashMap AccountNumber Account
-- createAccountLookup accounts =
--   HM.fromList $ Prelude.map (\account -> (account.accountNumber, account)) accounts

-- processTransactions :: HashMap AccountNumber Account -> [Transaction] -> ([String], (HashMap AccountNumber Account))
-- processTransactions accounts transactions =
--   let
--     getAccountNumber (Bill {accountNumber}) = accountNumber
--     getAccountNumber (Payment {accountNumber}) = accountNumber

--     applyTransaction :: HashMap AccountNumber Account -> Transaction -> Maybe Account
--     applyTransaction accts transaction = do
--       acct <- HM.lookup (getAccountNumber transaction) accts
--       processTransaction currencyConversionLookup transaction acct

--     applyResult 
--       :: [String] 
--       -> HashMap AccountNumber Account 
--       -> Transaction 
--       -> Maybe Account 
--       -> ([String], (HashMap AccountNumber Account))
--     applyResult errors accounts transaction (Just account) =
--       (errors, insert account.accountNumber account accounts)
--     applyResult errors accounts transaction Nothing =
--       (errors `snoc` ("Failed to process transaction: " <> show transaction), accounts)

--     buildResult 
--       :: ([String], (HashMap AccountNumber Account))
--       -> Transaction
--       -> ([String], (HashMap AccountNumber Account))
--     buildResult (errors, accounts) transaction =
--       applyTransaction accounts transaction |>
--         applyResult errors accounts transaction

--   in 
--   foldl 
--     buildResult
--     ([], accounts)
--     transactions

-- processTransaction :: HashMap Currency (HashMap Currency Int) -> Transaction -> Account -> Maybe Account
-- processTransaction currencyConversionLookup transaction account = do
--   let 
--     -- replace this with the 'amount' lens
--     transactionAmount = 
--       case transaction of
--         Bill { amount } -> amount
--         Payment { amount } -> amount
--     transactionOperation = 
--       case transaction of
--         Bill { amount } -> (_ + amount.amount)
--         Payment { amount } -> (_ - amount.amount)
--   rate <- convert currencyConversionLookup transactionAmount account.balance.currency
--   pure <| account { balance = account.balance { amount = transactionOperation account.balance.amount }}
