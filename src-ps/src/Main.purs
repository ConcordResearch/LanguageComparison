module Main where

import Prelude

import Data.Array (index, partition, length, snoc, filter, nub)
import Data.Either (Either(..), either, isRight, isLeft)
import Data.Foldable (foldl)
import Data.Function (apply, applyFlipped)
import Data.Int (fromString) as DataInt
import Data.List.Lazy (elemLastIndex)
import Data.Map (Map, fromFoldable, insert, lookup, values)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (fromString) as DataNumber
import Data.String (Pattern(..))
import Data.String.Common (split)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (Tuple3(..), (/\))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

infixr 0 apply as <|
infixl 0 applyFlipped as |>

rights :: forall a b. Array (Either a b) -> Array b
rights array = 
  let
    addValidElement :: forall a b. Array b -> Either a b -> Array b
    addValidElement accumulator (Left _) = accumulator
    addValidElement accumulator (Right value) = snoc accumulator value
  in
    foldl 
      addValidElement
      []
      array

lefts :: forall a b. Array (Either a b) -> Array a
lefts array = 
  let
    addValidElement :: forall a b. Array a -> Either a b -> Array a
    addValidElement accumulator (Left value) = snoc accumulator value
    addValidElement accumulator (Right _) = accumulator 
  in
    foldl 
      addValidElement
      [] 
      array

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
    currencyLookup = foldl (\accumulator currency -> insert currency Map.empty accumulator) Map.empty currencies
    
    insertRateAndInverse :: Map Currency (Map Currency Number) -> Tuple3 Currency Currency Number -> Maybe (Map Currency (Map Currency Number))
    insertRateAndInverse currencyMap (from /\ to /\ rate /\ unit) = do  
      fromCurrencyMap <- lookup from currencyMap
      toCurrencyMap <- lookup to currencyMap
      let newFromMap = insert to rate fromCurrencyMap :: Map Currency Number
      let newToMap = insert from (1.0/rate) toCurrencyMap

      insert from newFromMap currencyMap
        |> insert to newToMap 
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


--  To run:
--    $ node -e "require('./output/Main').main()"

main :: Effect Unit
main = do

  -- accountsText <- readTextFile UTF8 "accounts-1m.txt"
  -- log "Read Accounts Complete"
  -- log "start split lines"
  -- let splitLines = split (Pattern "\n") accountsText
  -- log "split lines complete"
  -- let maybeAccounts = map parseAccount splitLines
  -- log ("map complete: " <> (show $ length maybeAccounts))
  -- -- Theres something wrong with traverse over a large dataset
  --_ <- traverse ( show >>> log ) (maybeAccounts)  

  ---
  accountsText <- readTextFile UTF8 "accounts-1m.txt"
  log "Read Accounts Complete"
  transactionsText <- readTextFile UTF8 "transactions.txt"  
  log "Read Transactions Complete"

  let accounts = parseAccounts accountsText

  let accountErrors = lefts accounts
  let validAccounts = rights accounts
  log "Parse Accounts complete"
  let accountLookup = createAccountLookup validAccounts
  log "Create Account Lookup complete"
  
  let transactions = parseTransactions transactionsText
  let transactionErrors = lefts transactions
  let validTransactions = rights transactions
  log "Parse Transaction complete"
  
  let (errors /\ accountMap) = processTransactions accountLookup validTransactions
  --_ <- traverse ( show >>> log ) (values accountMap)  
  -- log ""
  pure unit

  -- log (show { accountNumber: "12345", balance: { amount: 100.0, currency: USD}, name: "Joe Smith"})

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
    currencySpecificConversionRates <- lookup money.currency conversionRates
    conversionRate <- lookup targetCurrency currencySpecificConversionRates
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

-- parseTransactions :: String -> Array (Either String Transaction)
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

createAccountLookup :: Array Account -> Map AccountNumber Account
createAccountLookup accounts =
  foldl (\acc acct -> insert acct.accountNumber acct acc) Map.empty accounts

-- > processTransactions
processTransactions :: Map AccountNumber Account -> Array Transaction -> Tuple (Array String) (Map AccountNumber Account)
processTransactions accounts transactions =
  let
    getAccountNumber (Bill {accountNumber}) = accountNumber
    getAccountNumber (Payment {accountNumber}) = accountNumber

    applyTransaction :: Map AccountNumber Account -> Transaction -> Maybe Account
    applyTransaction accts transaction = do
      acct <- lookup (getAccountNumber transaction) accts
      processTransaction currencyConversionLookup transaction acct

    applyResult 
      :: Array String 
      -> Map AccountNumber Account 
      -> Transaction 
      -> Maybe Account 
      -> Tuple (Array String) (Map AccountNumber Account)
    applyResult errors accounts transaction (Just account) =
      errors /\ insert account.accountNumber account accounts
    applyResult errors accounts transaction Nothing =
      errors `snoc` ("Failed to process transaction: " <> show transaction) /\ accounts

    buildResult 
      :: Tuple (Array String) (Map AccountNumber Account) 
      -> Transaction 
      -> Tuple (Array String) (Map AccountNumber Account)
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


