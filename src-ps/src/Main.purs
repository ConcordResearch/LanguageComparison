module Main where

import Prelude

import Data.Array (index, partition, length, snoc, filter, nub)
import Data.Either (Either(..), either, isRight, isLeft)
import Data.Foldable (foldl)
import Data.Function (apply, applyFlipped)
import Data.Int (fromString) as DataInt
import Data.List.Lazy (elemLastIndex)
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (fromString) as DataNumber
import Data.String (Pattern(..))
import Data.String.Common (split)
import Data.Traversable (traverse, sequence)
import Data.Tuple (fst, snd)
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
bill = Bill { accountNumber: "123", amount: { amount: 50.0, currency: USD}, bucket: "Dues"}

main :: Effect Unit
main = do
  -- bind :: (Monad m) => m a -> (a -> m b) -> m b
  -- readTextFile :: String -> Effect String
  -- log :: String -> Effect ()
  -- readTextFile UTF8 "accounts.txt" `bind` log

  accountsText <- readTextFile UTF8 "accounts.txt"
  transactionsText <- readTextFile UTF8 "transactions.txt"  
  
  let accounts = parseAccounts accountsText
  let accountErrors = lefts accounts
  let validAccounts = rights accounts
  
  --Array (Either String Account) ->  () -> Effect ()
  -- Array (Effect ())
  --traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)

  -- _ <- traverse (either log (show >>> log)) validAccounts.yes
  -- _ <- traverse (either log (show >>> log)) validAccounts.no

  let transactions = parseTransactions transactionsText
  let transactionErrors = lefts transactions
  let validTransactions = rights transactions
  
  -- t (m a) -> m (t a)
  -- Array (Either String Transaction) -> Either String (Array Transaction)
  -- sequence validTransactions.yes
  -- _ <- traverse (either log (show >>> log)) validTransactions.yes
  -- _ <- traverse (either log (show >>> log)) validTransactions.no
  
  

  -- log <| "Valid accounts: " <> show (length validAccounts.yes)
  -- log <| "Valid transactions: " <> show (length validTransactions.yes)

  -- let newAccounts = processTransactions validAccounts validTransactions
  -- map show newAccounts |> log
  log "foo"
  
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

-- combineMoney :: Money -> Money -> Maybe Money
-- combineMoney first second =
--   if first.currency == second.currency 
--   then Just <| first { amount = first.amount + second.amount }
--   else Nothing

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

processTransactions :: Map AccountNumber Account -> Array Transaction -> Map AccountNumber Account
processTransactions accounts transactions = do
  
  -- this results in 
  foldl 
    (\(errors /\ accounts) transaction -> 
      let
        case 
          lookup transaction.accountNumber accounts 
          >>= processTransaction currencyConversionLookup tranaction of
          Just account -> (errors /\ insert account.accountNumber account accounts)
          Nothing -> (errors <> ("Failed to process transaction: " <> show transaction) /\ accounts)
    )
    ([] /\ accounts)
    transactions
  
--   let

--     f acc transaction = transaction.accountNumber
--     account = get accounts payment.accountNumber
--     newAccount = map ? account
    
--   in
--     foldl f mempty transactions
--     set accounts payment.accountNumber newAccount
-- processTransactions accounts (Bill bill) =

processTransaction :: Map Currency (Map Currency Number) -> Transaction -> Account -> Maybe Account
processTransaction currencyConversionLookup transaction account = do
    --get account from accounts
    -- account <- lookup  accountNumber accounts
    
    --case to get the transaction currency out
    let 
      transactionAmount = 
        case transaction of
          Bill { amount } -> amount
          Payment { amount } -> amount
      transactionOperation = 
        case transaction of
          Bill { amount } -> (_ + amount.amount)
          Payment { amount } -> (_ - amount.amount)
    --get conversion rate
    rate <- convert currencyConversionLookup transactionAmount account.balance.currency
    --apply conversion rate to transaction

    pure <| account { balance = account.balance { amount = transactionOperation account.balance.amount }}

    --pure <| insert accountNumber updatedAccount account
