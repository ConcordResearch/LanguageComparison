module Main where

import Prelude

import Control.Monad.ST.Internal (ST, foreach)
import Data.Array (index, partition, length, snoc, filter, nub, take, cons, fromFoldable, singleton, mapMaybe)
import Data.Array.ST (empty, push, freeze, unsafeFreeze)
import Data.Either (Either(..), either, isRight, isLeft, hush)
import Data.Foldable (foldl, foldr)
import Data.Function (apply, applyFlipped)
import Data.Int (fromString) as DataInt
-- import Data.Interval (DurationComponent(..))
import Data.List.Lazy (elemLastIndex)
import Data.Map (Map, insert, lookup, values)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (fromString) as DataNumber
import Data.String (Pattern(..))
import Data.String.Common (split)
import Data.Time (Millisecond, Second, diff)
import Data.Time.Duration (class Duration, Milliseconds, fromDuration)
import Data.Traversable (traverse, sequence)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (Tuple3(..), (/\))
import Effect (Effect, foreachE)
import Effect.Console (log)
import Effect.Exception (throwException)
import Effect.Now (nowTime)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

foreign import now :: Effect Number

infixr 0 apply as <|
infixl 0 applyFlipped as |>

-- rights :: forall a b. Array (Either a b) -> Array b
-- rights array =
--   let
--     f (Left _) accumulator = accumulator
--     -- Both `snoc` and `<>` perform very poorly
--     -- maybe look at https://pursuit.purescript.org/packages/purescript-rrb-list/0.0.1
--     f (Right value) accumulator = snoc accumulator value
--     -- f (Right value) accumulator = accumulator <> singleton value
--   in
--     foldr f [] array


-- import Data.Array (snoc)
-- import Data.Either (Either(..))

-- rights :: forall a b. Array (Either a b) -> Array b
-- rights array =
--   let
--     f (Left _) accumulator = accumulator
--     f (Right value) accumulator = snoc accumulator value
--   in
--     foldr f [] array

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

  -- import Data.Time.Duration (class Duration, Milliseconds(..), fromDuration)


  -- log $ diff 
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
  
  -- This was REALLY slow cause of `snoc` in the rights/lefts
  -- let accountErrors = lefts accounts
  let validAccounts = rights accounts
  tt4 <- now
  log <| "Accounts Right Complete " <> (show <| tt4 - tt3)
  
  let accountLookup = createAccountLookup validAccounts
  tt5 <- now
  log <| "Create Account Lookup complete " <> (show <| tt5 - tt4)
  
  let transactions = parseTransactions transactionsText
  -- let transactionErrors = lefts transactions
  let validTransactions = rights transactions
  tt6 <- now
  log <| "Parse Transaction Complete " <> (show <| tt6 - tt5)
  
  let (errors /\ accountMap) = processTransactions accountLookup validTransactions
  tt7 <- now
  log <| "Process Transactions Complete " <> (show <| tt7 - tt6)

  --the use of `traverse` was very slow. using foreachE solves it
  --_ <- traverse ( show >>> log ) (values accountMap)  
  foreachE (fromFoldable $ values accountMap) ( show >>> log )
  
  tt8 <- now
  
  
  
  log <| "Read Accounts Complete " <> (show <| tt1 - tt0)
  log <| "Read Transactions Complete " <> (show <| tt2 - tt1)
  log <| "Parse Accounts complete " <> (show <| tt3 - tt2)
  log <| "Accounts Right Complete " <> (show <| tt4 - tt3)
  log <| "Create Account Lookup complete " <> (show <| tt5 - tt4)
  log <| "Parse Transaction Complete " <> (show <| tt6 - tt5)
  log <| "Process Transactions Complete " <> (show <| tt7 - tt6)
  log <| "Complete " <> (show <| tt8 - tt7)
  pure unit

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


