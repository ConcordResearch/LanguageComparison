{-# LANGUAGE ExplicitForAll #-}
-- https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Implementation
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Lib where

import Prelude hiding (log, (!!), lines, readFile, writeFile, putStrLn, appendFile)
import qualified Prelude
import Control.Category ((<<<), (>>>))
import Data.Traversable (mapM)
import Data.Either (rights, lefts)
import Data.Either.Combinators (rightToMaybe)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap, insert, lookup, elems)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.List (nub)
import Data.Text (Text, splitOn, unpack, pack)
import Data.Text.IO (readFile, writeFile, appendFile, putStrLn)
-- import Text.Read (readMay)
import qualified Text.Read
import qualified Data.Attoparsec.Text as Attoparsec
import TextShow

import Data.Tuple ( fst)
import Control.Lens hiding ((<|), (|>), from, to)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Maybe(Maybe)
import Data.Time.Clock (NominalDiffTime, getCurrentTime, diffUTCTime)

import Data

(<|) :: (a -> b) -> a -> b
(<|) = ($)
infixr 0 <|
(|>) :: a -> (a -> b) -> b
(|>) = (&)
infixl 0 |>

tryParseInt :: Text -> Maybe Int
tryParseInt text = 
  case Attoparsec.parseOnly Attoparsec.double text of
    Right r -> Just $ (Prelude.round r :: Int)
    _ -> Nothing

-- https://hackage.haskell.org/package/basic-prelude-0.7.0/docs/src/BasicPrelude.html#tshow
-- show :: Show a => a -> Text
-- show = pack . Prelude.show

-- https://github.com/qfpl/papa/blob/536b0a9243802347c299e077b5d85beb80d3a4a1/papa-lens-implement/src/Papa/Lens/Implement/Data/List.hs
(!!) ::
  Ixed s =>
  s
  -> Index s
  -> Maybe (IxValue s)
q !! n =
  q ^? ix n

infixl 9 !!

log :: Text -> IO ()
-- log = Prelude.show >>> putStrLn
log = putStrLn

instance TextShow NominalDiffTime where
  showt = pack . Prelude.show
  showb = fromText . showt

-- |  To run:
-- |  $ node -e "require('./output/Main').main()"
run :: IO ()
run = do
  log ""

  tt0 <- getCurrentTime
  accountsText <- readFile "../accounts1.2m.txt" 
  tt1 <- getCurrentTime
  log <| "Read Accounts Complete " <> (showt <| diffUTCTime tt1  tt0)
  transactionsText <- readFile "../transactions10m.txt"
  tt2 <- getCurrentTime
  log <| "Read Transactions Complete " <> (showt <| diffUTCTime tt2 tt1)

  let accounts = parseAccounts accountsText
  tt3 <- getCurrentTime
  log <| "Parse Accounts complete " <> (showt <| diffUTCTime tt3 tt2)
  
  let validAccounts = rights accounts
  tt4 <- getCurrentTime
  log <| "Accounts Right Complete " <> (showt <| diffUTCTime tt4 tt3)
  
  let accountLookup = createAccountLookup validAccounts
  tt5 <- getCurrentTime
  log <| "Create Account Lookup complete " <> (showt <| diffUTCTime tt5 tt4)
  
  let transactions = parseTransactions transactionsText
  let validTransactions = rights transactions
  tt6 <- getCurrentTime
  log <| "Parse Transaction Complete " <> (showt <| diffUTCTime tt6 tt5)
  
  let (errors, acctMap) = processTransactions accountLookup validTransactions
  tt7 <- getCurrentTime
  log <| "Process Transactions Complete " <> (showt <| diffUTCTime tt7 tt6)

  -- let processedValues = elems acctMap
  tt8 <- getCurrentTime
  log <| "Values ToArray Complete " <> (showt <| diffUTCTime tt8 tt7)

  -- writes to stdout
  -- printT $ showbList $ elems acctMap
  
  writeFile "output.txt" $ toResults acctMap
  -- mapM_ (Prelude.appendFile "output.txt") $ map Prelude.show $ elems acctMap



  tt9 <- getCurrentTime

  log <| "Read Accounts Complete " <> (showt <| diffUTCTime tt1 tt0)
  log <| "Read Transactions Complete " <> (showt <| diffUTCTime tt2 tt1)
  log <| "Parse Accounts complete " <> (showt <| diffUTCTime tt3 tt2)
  log <| "Accounts Right Complete " <> (showt <| diffUTCTime tt4 tt3)
  log <| "Create Account Lookup complete " <> (showt <| diffUTCTime tt5 tt4)
  log <| "Parse Transaction Complete " <> (showt <| diffUTCTime tt6 tt5)
  log <| "Process Transactions Complete " <> (showt <| diffUTCTime tt7 tt6)
  log <| "Values ToArray Complete " <> (showt <| diffUTCTime tt8 tt7)
  log <| "Complete " <> (showt <| diffUTCTime tt9 tt8)
  log <| "Total " <> (showt <| diffUTCTime tt9 tt0)
  pure ()

toResults :: (HashMap AccountNumber Account) -> Text
toResults acctMap = toText $ showbList $ elems acctMap


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

parseAccounts :: Text -> [Either Text Account]
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

toMaybeTuple :: [a] -> Maybe (a,a)
toMaybeTuple [a,b] = Just (a,b)
toMaybeTuple _ = Nothing

toMaybeTuple3 :: [a] -> Maybe (a,a,a)
toMaybeTuple3 [a,b,c] = Just (a,b,c)
toMaybeTuple3 _ = Nothing

toMaybeTuple4 :: [a] -> Maybe (a,a,a,a)
toMaybeTuple4 [a,b,c,d] = Just (a,b,c,d)
toMaybeTuple4 _ = Nothing

parseAccount :: Text -> Maybe Account
parseAccount text = do
  (a,b,name) <- toMaybeTuple3 $ splitOn "|" text
  acctNum <- getAccountNumber a
  balance <- getAmount b
  pure $ Account acctNum balance name
  
getAccountNumber :: Text -> Maybe AccountNumber
getAccountNumber s = do
  num <- tryParseInt s :: Maybe Int
  pure $ (AccountNumber (showt num))

getAmount :: Text -> Maybe Money
getAmount amountText = do
  (firstText, currencyText) <- toMaybeTuple $ splitOn " " amountText
  val <- tryParseInt firstText :: Maybe Int
  currency' <- parseCurrency currencyText
  pure $ Money val currency'

parseCurrency :: Text -> Maybe Currency
parseCurrency "USD" = Just USD
parseCurrency "MXN" = Just MXN
parseCurrency "GBP" = Just GBP
parseCurrency "EUD" = Just EUD
parseCurrency "THB" = Just THB
parseCurrency _ = Nothing

parseTransactions :: Text -> [Either Text Transaction]
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

parseTransaction :: Text -> Maybe Transaction
parseTransaction text = do
  (a,b,transtype,transDetails) <- toMaybeTuple4 $ splitOn "|" text
  acctNum <- getAccountNumber a
  amt <- getAmount b
  case transtype of
    "Bill" -> pure $ Bill acctNum amt transDetails
    "Payment" -> pure $ Payment acctNum amt transDetails
    _ -> Nothing

createAccountLookup :: [Account] -> HashMap AccountNumber Account
createAccountLookup accts =
  HM.fromList $ Prelude.map (\acct -> (acct^.accountNumber, acct)) accts

processTransactions :: HashMap AccountNumber Account -> [Transaction] -> ([Text], (HashMap AccountNumber Account))
processTransactions accounts transactions =
  let
    buildResult :: ([Text], (HashMap AccountNumber Account)) -> Transaction -> ([Text], (HashMap AccountNumber Account))
    buildResult (errors, accounts') transaction = 
      let 
        result =
          (HM.lookup (transaction^.accountNumber) accounts')
            >>= (\acct -> pure $ applyTransaction acct transaction)
        add acct = insert (acct^.accountNumber) acct accounts'
        errors' = ("Failed to process transaction: " <> showt transaction) : errors
      in
        case result of
          Just newAccount -> (errors, add newAccount)
          Nothing -> (errors', accounts')
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
applyTransaction :: Account -> Transaction -> Account
applyTransaction a t =
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
