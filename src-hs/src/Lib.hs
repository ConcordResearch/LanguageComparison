module Lib where

import Prelude

import Data.Array (index, snoc, nub, fromFoldable, mapMaybe)
import Data.Either (Either(..), either, hush)
import Data.Foldable (foldl)
import qualified Data.Function as Function
import qualified Data.Int (fromString)
import Data.Map (Map)
import Data.Map as Map
import Data.HashMap (HashMap, insert, lookup, values)
import Data.HashMap as HM
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import qualified Data.Number (fromString)
import Data.String (Pattern(..))
import Data.String.Common (split)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (Tuple3, (/\))
import Effect (Effect, foreachE)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Time (now)

-- infixr 0 Function.apply as <|
-- infixl 0 Function.applyFlipped as |>

-- |  To run:
-- |  $ node -e "require('./output/Main').main()"
main :: Effect Unit
main = do

  tt0 <- now
  accountsText <- readTextFile UTF8 "accounts-1m.txt"
  tt1 <- now
  log <| "Read Accounts Complete " <> (show <| tt1 - tt0)
  transactionsText <- readTextFile UTF8 "transactions-1m.txt"
  tt2 <- now
  log <| "Read Transactions Complete " <> (show <| tt2 - tt1)

  let accounts = parseAccounts accountsText
  tt3 <- now
  log <| "Parse Accounts complete " <> (show <| tt3 - tt2)
  
  let validAccounts = rights accounts
  tt4 <- now
  log <| "Accounts Right Complete " <> (show <| tt4 - tt3)
  
  let accountLookup = createAccountLookup validAccounts
  tt5 <- now
  log <| "Create Account Lookup complete " <> (show <| tt5 - tt4)
  
  let transactions = parseTransactions transactionsText
  let validTransactions = rights transactions
  tt6 <- now
  log <| "Parse Transaction Complete " <> (show <| tt6 - tt5)
  
  let (errors /\ accountMap) = processTransactions accountLookup validTransactions
  tt7 <- now
  log <| "Process Transactions Complete " <> (show <| tt7 - tt6)

  let processedValues = fromFoldable $ values accountMap
  tt8 <- now
  log <| "Values ToArray Complete " <> (show <| tt8 - tt7)

  foreachE processedValues ( show >>> log )
  tt9 <- now

  log <| "Read Accounts Complete " <> (show <| tt1 - tt0)
  log <| "Read Transactions Complete " <> (show <| tt2 - tt1)
  log <| "Parse Accounts complete " <> (show <| tt3 - tt2)
  log <| "Accounts Right Complete " <> (show <| tt4 - tt3)
  log <| "Create Account Lookup complete " <> (show <| tt5 - tt4)
  log <| "Parse Transaction Complete " <> (show <| tt6 - tt5)
  log <| "Process Transactions Complete " <> (show <| tt7 - tt6)
  log <| "Values ToArray Complete " <> (show <| tt8 - tt7)
  log <| "Complete " <> (show <| tt9 - tt8)
  log <| "Total " <> (show <| tt9 - tt0)
  pure unit


rights :: forall a b. Array (Either a b) -> Array b
rights array = mapMaybe hush array

lefts :: forall a b. Array (Either a b) -> Array a
lefts array = mapMaybe (either Just (const Nothing)) array

conversionRates :: Array (Tuple3 Currency Currency Number)
conversionRates =
    [ USD /\ MXN /\ 1.5 /\ unit
    , USD /\ EUD /\ 2.5 /\ unit 
    , USD /\ THB /\ 3.5 /\ unit 
    , USD /\ GBP /\ 4.5 /\ unit 
    , MXN /\ EUD /\ 2.5 /\ unit 
    , MXN /\ THB /\ 3.5 /\ unit 
    , MXN /\ GBP /\ 4.5 /\ unit 
    , EUD /\ THB /\ 3.5 /\ unit 
    , EUD /\ GBP /\ 4.5 /\ unit 
    , THB /\ GBP /\ 4.5 /\ unit 
    ]

currencyConversionLookup :: Map Currency (Map Currency Number)
currencyConversionLookup = makeCurrencyConversionLookup conversionRates

makeCurrencyConversionLookup :: Array (Tuple3 Currency Currency Number) -> Map Currency (Map Currency Number)
makeCurrencyConversionLookup currencyMappings =
  let
    currencies :: Array Currency
    currencies = nub <| map fst currencyMappings <> map (\(a /\ b /\ c) -> b) currencyMappings
    
    currencyLookup :: Map Currency (Map Currency Number)
    currencyLookup = foldl (\accumulator currency -> Map.insert currency Map.empty accumulator) Map.empty currencies
    
    insertRateAndInverse :: Map Currency (Map Currency Number) -> Tuple3 Currency Currency Number -> Maybe (Map Currency (Map Currency Number))
    insertRateAndInverse currencyMap (from /\ to /\ rate /\ unit) = do  
      fromCurrencyMap <- Map.lookup from currencyMap
      toCurrencyMap <- Map.lookup to currencyMap
      let newFromMap = Map.insert to rate fromCurrencyMap :: Map Currency Number
      let newToMap = Map.insert from (1.0/rate) toCurrencyMap

      Map.insert from newFromMap currencyMap
        |> Map.insert to newToMap 
        |> pure
  in
    foldl 
      (\allMappings conversionRate -> 
        fromMaybe allMappings (insertRateAndInverse allMappings conversionRate)
      ) 
      currencyLookup 
      currencyMappings

account = { accountNumber: "123", balance: { amount: 100.0, currency: USD}, name: "John Doe"}
payment = Payment { accountNumber: "123", amount: { amount: 50.0, currency: USD}, source: "Online Payment"}
bill = Bill { accountNumber: "123", amount: { amount: 25.0, currency: USD}, bucket: "Dues"}

accountMap = createAccountLookup [account]
transactionList = [payment, bill, payment]

data Currency = USD | MXN | EUD | THB | GBP 
instance showCurrency :: Show Currency where
  show USD = "USD"
  show MXN = "MXN"
  show EUD = "EUD"
  show THB = "THB"
  show GBP = "GBP"
derive instance eqCurrency :: Eq Currency
derive instance ordCurrency :: Ord Currency

type Money = 
  { amount :: Number
  , currency :: Currency
  }

convert :: Map Currency (Map Currency Number) -> Money -> Currency -> Maybe Money
convert conversionRates money targetCurrency =
  if money.currency == targetCurrency
  then Just <| money
  else do
    currencySpecificConversionRates <- Map.lookup money.currency conversionRates
    conversionRate <- Map.lookup targetCurrency currencySpecificConversionRates
    Just <| { amount: money.amount * conversionRate, currency: targetCurrency }

type AccountNumber = String

type Account =
  { accountNumber :: AccountNumber
  , balance :: Money
  , name :: String
  }

data Transaction
  = Bill 
    { accountNumber :: AccountNumber
    , amount :: Money
    , bucket :: String
    }
  | Payment
    { accountNumber :: AccountNumber
    , amount :: Money
    , source :: String
    }
instance showTransaction :: Show Transaction where
  show (Bill {accountNumber, amount, bucket}) = 
    "Bill - accountNumber: " 
      <> (show accountNumber) 
      <> ", amount: "
      <> (show amount)
      <> ", bucket: "
      <> bucket
  show (Payment {accountNumber, amount, source}) = 
    "Bill - accountNumber: " 
      <> (show accountNumber) 
      <> ", amount: "
      <> (show amount)
      <> ", source: "
      <> source

parseAccounts :: String -> Array (Either String Account)
parseAccounts text =
  let
    lines = split (Pattern "\n") text
    lineOrErrorMessage line = 
      maybe 
        (Left <| "Error parsing account line: " <> line) 
        Right
        (parseAccount line)
  in
    map lineOrErrorMessage lines

parseAccount :: String -> Maybe Account
parseAccount text = do
  let fields = split (Pattern "|") text
  accountNumber <- getAccountNumber fields 0
  balance <- getAmount fields 1
  name <- index fields 2 
  pure { accountNumber, balance, name}

getAccountNumber :: Array String -> Int -> Maybe AccountNumber
getAccountNumber fields i =
  index fields i 
    >>= DataInt.fromString 
    >>= show >>> pure
  -- this is the same thing using do notation instead of bind (aka >>=) directly
  -- do
  --   accountText <- index fields i
  --   accountNumber <- DataInt.fromString accountText
  --   pure <| show accountNumber

getAmount :: Array String -> Int -> Maybe Money
getAmount fields i = do
  amountText <- index fields i 
  let amountParts = split (Pattern " ") amountText
  firstText <- index amountParts 0 
  currencyText <- index amountParts 1 
  amount <- DataNumber.fromString firstText
  currency <- parseCurrency currencyText  
  pure ({amount, currency})

parseCurrency :: String -> Maybe Currency
parseCurrency "USD" = Just USD
parseCurrency "MXN" = Just MXN
parseCurrency "GBP" = Just GBP
parseCurrency "EUD" = Just EUD
parseCurrency "THB" = Just THB
parseCurrency _ = Nothing

parseTransactions :: String -> Array (Either String Transaction)
parseTransactions text =
  let
    lines = split (Pattern "\n") text
    lineOrErrorMessage line = 
      maybe 
        (Left <| "Error parsing transaction line: " <> line)
        Right
        (parseTransaction line)
  in
    map lineOrErrorMessage lines

parseTransaction :: String -> Maybe Transaction
parseTransaction text = do
  let fields = split (Pattern "|") text
  accountNumber <- getAccountNumber fields 0
  amount <- getAmount fields 1
  transtype <- index fields 2
  transDetails <- index fields 3
  case transtype of
    "Bill" -> pure <| Bill { accountNumber, amount, bucket: transDetails}
    "Payment" -> pure <| Payment { accountNumber, amount, source: transDetails}
    _ -> Nothing

createAccountLookup :: Array Account -> HashMap AccountNumber Account
createAccountLookup accounts =
  HM.fromFoldable $ map (\account -> Tuple account.accountNumber account) accounts

processTransactions :: HashMap AccountNumber Account -> Array Transaction -> Tuple (Array String) (HashMap AccountNumber Account)
processTransactions accounts transactions =
  let
    getAccountNumber (Bill {accountNumber}) = accountNumber
    getAccountNumber (Payment {accountNumber}) = accountNumber

    applyTransaction :: HashMap AccountNumber Account -> Transaction -> Maybe Account
    applyTransaction accts transaction = do
      acct <- lookup (getAccountNumber transaction) accts
      processTransaction currencyConversionLookup transaction acct

    applyResult 
      :: Array String 
      -> HashMap AccountNumber Account 
      -> Transaction 
      -> Maybe Account 
      -> Tuple (Array String) (HashMap AccountNumber Account)
    applyResult errors accounts transaction (Just account) =
      errors /\ insert account.accountNumber account accounts
    applyResult errors accounts transaction Nothing =
      errors `snoc` ("Failed to process transaction: " <> show transaction) /\ accounts

    buildResult 
      :: Tuple (Array String) (HashMap AccountNumber Account) 
      -> Transaction 
      -> Tuple (Array String) (HashMap AccountNumber Account)
    buildResult (errors /\ accounts) transaction =
      applyTransaction accounts transaction |>
        applyResult errors accounts transaction

  in 
  foldl 
    buildResult
    ([] /\ accounts)
    transactions

processTransaction :: Map Currency (Map Currency Number) -> Transaction -> Account -> Maybe Account
processTransaction currencyConversionLookup transaction account = do
  let 
    transactionAmount = 
      case transaction of
        Bill { amount } -> amount
        Payment { amount } -> amount
    transactionOperation = 
      case transaction of
        Bill { amount } -> (_ + amount.amount)
        Payment { amount } -> (_ - amount.amount)
  rate <- convert currencyConversionLookup transactionAmount account.balance.currency
  pure <| account { balance = account.balance { amount = transactionOperation account.balance.amount }}
