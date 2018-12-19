{-# LANGUAGE ExplicitForAll #-}
-- https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Implementation
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lib where

import Prelude hiding ((.), log, (!!), lines, readFile)
import Control.Category ((<<<), (>>>))
import Data.Traversable (mapM)
import Data.Either (rights, lefts)
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (foldl')
import Data.HashMap.Lazy (HashMap, insert, lookup, elems)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Tuple ( fst)
import Control.Lens hiding ((<|), (|>), from, to)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Maybe(Maybe)
import System.IO.Strict (readFile)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Data

(<|) :: (a -> b) -> a -> b
(<|) = ($)
infixr 0 <|
(|>) :: a -> (a -> b) -> b
(|>) = (&)
infixl 0 |>

--helps with dot notation
-- https://ghc.haskell.org/trac/ghc/ticket/14812
-- (.) = flip ($)

-- concatMap :: forall a b. (a -> [b]) -> [a] -> [b]
-- concatMap = flip bind

-- singleton :: forall a. a -> [a]
-- singleton a = [a]

-- mapMaybe :: forall a b. (a -> Maybe b) -> [a] -> [b]
-- mapMaybe f = concatMap (maybe [] singleton <<< f)

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
run :: IO ()
run = do
  log ""

  tt0 <- getCurrentTime
  accountsText <- readFile "accounts-1m.txt" 
  tt1 <- getCurrentTime
  log <| "Read Accounts Complete " <> (show <| diffUTCTime tt1  tt0)
  transactionsText <- readFile "transactions-1m.txt"
  tt2 <- getCurrentTime
  log <| "Read Transactions Complete " <> (show <| diffUTCTime tt2 tt1)

  let accounts = parseAccounts accountsText
  tt3 <- getCurrentTime
  log <| "Parse Accounts complete " <> (show <| diffUTCTime tt3 tt2)
  
  let validAccounts = rights accounts
  tt4 <- getCurrentTime
  log <| "Accounts Right Complete " <> (show <| diffUTCTime tt4 tt3)
  
  let accountLookup = createAccountLookup validAccounts
  tt5 <- getCurrentTime
  log <| "Create Account Lookup complete " <> (show <| diffUTCTime tt5 tt4)
  
  let transactions = parseTransactions transactionsText
  let validTransactions = rights transactions
  tt6 <- getCurrentTime
  log <| "Parse Transaction Complete " <> (show <| diffUTCTime tt6 tt5)
  
  let (errors, acctMap) = processTransactions accountLookup validTransactions
  tt7 <- getCurrentTime
  log <| "Process Transactions Complete " <> (show <| diffUTCTime tt7 tt6)

  let processedValues = elems acctMap
  tt8 <- getCurrentTime
  log <| "Values ToArray Complete " <> (show <| diffUTCTime tt8 tt7)



  traverse ( show >>> log ) processedValues

  -- let newContents = map show processedValues
  -- when (length newContents > 0) $
  --   writeFile "file.txt" newContents

  tt9 <- getCurrentTime

  log <| "Read Accounts Complete " <> (show <| diffUTCTime tt1 tt0)
  log <| "Read Transactions Complete " <> (show <| diffUTCTime tt2 tt1)
  log <| "Parse Accounts complete " <> (show <| diffUTCTime tt3 tt2)
  log <| "Accounts Right Complete " <> (show <| diffUTCTime tt4 tt3)
  log <| "Create Account Lookup complete " <> (show <| diffUTCTime tt5 tt4)
  log <| "Parse Transaction Complete " <> (show <| diffUTCTime tt6 tt5)
  log <| "Process Transactions Complete " <> (show <| diffUTCTime tt7 tt6)
  log <| "Values ToArray Complete " <> (show <| diffUTCTime tt8 tt7)
  log <| "Complete " <> (show <| diffUTCTime tt9 tt8)
  log <| "Total " <> (show <| diffUTCTime tt9 tt0)
  pure ()

conversionRates :: [(Currency, Currency, Rational)]
conversionRates =
    [ (USD, MXN, 1.5)
    , (USD, EUD, 2.5) 
    , (USD, THB, 3.5) 
    , (USD, GBP, 4.5) 
    , (MXN, EUD, 2.5) 
    , (MXN, THB, 3.5) 
    , (MXN, GBP, 4.5) 
    , (EUD, THB, 3.5) 
    , (EUD, GBP, 4.5) 
    , (THB, GBP, 4.5) 
    ]

currencyConversionLookup :: HashMap Currency (HashMap Currency Rational)
currencyConversionLookup = makeCurrencyConversionLookup conversionRates

makeCurrencyConversionLookup :: [(Currency, Currency, Rational)] -> HashMap Currency (HashMap Currency Rational)
makeCurrencyConversionLookup currencyMappings =
  let
    currencies :: [Currency]
    currencies = nub <| (Prelude.map (\(a, _, _) -> a) currencyMappings) <> map (\(_, b, _) -> b) currencyMappings
    
    currencyLookup :: HashMap Currency (HashMap Currency Rational)
    currencyLookup = foldl' (\accumulator curr -> HM.insert curr HM.empty accumulator) HM.empty currencies
    
    insertRateAndInverse :: HashMap Currency (HashMap Currency Rational) -> (Currency, Currency, Rational) -> Maybe (HashMap Currency (HashMap Currency Rational))
    insertRateAndInverse currencyMap (from, to, rate) = do  
      -- lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
      fromCurrencyMap <- HM.lookup from currencyMap
      toCurrencyMap <- HM.lookup to currencyMap
      let newFromMap = HM.insert to rate fromCurrencyMap :: HashMap Currency Rational
      let newToMap = HM.insert from (1/rate) toCurrencyMap

      HM.insert from newFromMap currencyMap
        |> HM.insert to newToMap 
        |> pure
  in
    foldl'
    (\allMappings conversionRate -> 
        fromMaybe allMappings (insertRateAndInverse allMappings conversionRate)
    ) 
    currencyLookup 
    currencyMappings

account :: Account
account = Account (AccountNumber "123") (Money 100 USD) "John Doe"
payment :: Transaction
payment = Payment (AccountNumber "123") (Money 50 USD) "Online Payment"
bill :: Transaction
bill = Bill (AccountNumber "123") (Money 25 USD) "Dues"

accountMap :: HashMap AccountNumber Account
accountMap = createAccountLookup [account]
transactionList :: [Transaction]
transactionList = [payment, bill, payment]
      

parseAccounts :: String -> [Either String Account]
parseAccounts text =
  let
    lines = splitOn "\n" text
    lineOrErrorMessage line = 
      maybe 
        (Left <| "Error parsing account line: " <> line) 
        Right
        (parseAccount line)
  in
    Prelude.map lineOrErrorMessage lines

parseAccount :: String -> Maybe Account
parseAccount text = do
  let list = splitOn "|" text
  a <- list !! 0
  b <- list !! 1
  name <- list !! 2
  acctNum <- getAccountNumber a
  balance <- getAmount b
  pure $ Account acctNum balance name
  
getAccountNumber :: String -> Maybe AccountNumber
getAccountNumber s = do
  num <- readMaybe s :: Maybe Int
  pure (AccountNumber (show num))

getAmount :: String -> Maybe Money
getAmount amountText = do
  let amountParts = splitOn " " amountText
  firstText <- amountParts !! 0
  currencyText <- amountParts !! 1
  val <- readMaybe firstText :: Maybe Int
  currency' <- parseCurrency currencyText
  pure $ Money val currency'

parseCurrency :: String -> Maybe Currency
parseCurrency "USD" = Just USD
parseCurrency "MXN" = Just MXN
parseCurrency "GBP" = Just GBP
parseCurrency "EUD" = Just EUD
parseCurrency "THB" = Just THB
parseCurrency _ = Nothing

parseTransactions :: String -> [Either String Transaction]
parseTransactions text =
  let
    lines = splitOn "\n" text
    lineOrErrorMessage line = 
      maybe 
        (Left <| "Error parsing transaction line: " <> line)
        Right
        (parseTransaction line)
  in
    Prelude.map lineOrErrorMessage lines

parseTransaction :: String -> Maybe Transaction
parseTransaction text = do
  let fields = splitOn "|" text
  a <- fields !! 0
  b <- fields !! 1
  acctNum <- getAccountNumber a
  amt <- getAmount b
  transtype <- fields !! 2
  transDetails <- fields !! 3
  case transtype of
    "Bill" -> pure $! Bill acctNum amt transDetails
    "Payment" -> pure <| Payment acctNum amt transDetails
    _ -> Nothing

createAccountLookup :: [Account] -> HashMap AccountNumber Account
createAccountLookup accts =
  HM.fromList $! Prelude.map (\acct -> (acct^.accountNumber, acct)) accts

processTransactions :: HashMap AccountNumber Account -> [Transaction] -> ([String], (HashMap AccountNumber Account))
processTransactions accounts transactions =
  let
    applyTransaction :: HashMap AccountNumber Account -> Transaction -> Maybe Account
    applyTransaction accts transaction = do
      acct <- HM.lookup (transaction^.accountNumber) accts
      -- processTransaction currencyConversionLookup transaction acct
      Just $! appl acct transaction

    applyResult 
      :: [String] 
      -> HashMap AccountNumber Account 
      -> Transaction 
      -> Maybe Account 
      -> ([String], (HashMap AccountNumber Account))
    applyResult errors accts _ (Just acct) =
      (errors, insert (acct^.accountNumber) acct accts)
    applyResult errors accts trans Nothing =
      (("Failed to process transaction: " <> show trans) : errors, accts)

    buildResult 
      :: ([String], (HashMap AccountNumber Account))
      -> Transaction
      -> ([String], (HashMap AccountNumber Account))
    buildResult (errors, accts') transaction =
      applyTransaction accts' transaction |>
        applyResult errors accts' transaction
  in 
    foldl'
      buildResult
      ([], accounts)
      transactions

-- Add or subtract a transaction amount from account amount
-- > appl a1 t1
-- Account {_accountNumber = AccountNumber "345", _balance = 35 USD, _name = "Jorge"}
-- > appl a1 t2
-- Account {_accountNumber = AccountNumber "345", _balance = 10 USD, _name = "Jorge"}
appl :: Account -> Transaction -> Account
appl a t =
  let
    oper (Bill _ _ _) = addCurrency
    oper (Payment _ _ _) = minusCurrency
    newAcct = amount %~ (\bal -> oper t bal (t^.amount))
  in
    newAcct a

calcRate :: Currency -> Currency -> Int
calcRate USD USD = 1
calcRate THB USD = 2
calcRate _   _   = 3

-- Convert from one currency to another
-- > convert (Money 100 USD) USD
-- Just 100 USD
-- > convert (Money 100 USD) THB
-- Just 200 USD
convert :: Money -> Currency -> Money
convert m c =
  let 
    rate = calcRate (m^.currency) c
  in 
    value *~ rate $ m

-- > addCurrency (Money 100 USD) (Money 150 THB)
-- 400 THB
addCurrency :: Money -> Money -> Money
addCurrency m1 m2 =
  let
    newM2 = convert m2 (m1^.currency)
  in
    value %~ (\a -> m1^.value + a) $ newM2

minusCurrency :: Money -> Money -> Money
minusCurrency m1 m2 =
  let
    newM2 = convert m2 (m1^.currency)
  in
    value %~ (\a -> m1^.value - a ) $ newM2
