module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Function (apply, applyFlipped)
import Data.String.Common (split)
import Data.Int (fromString) as DataInt
import Data.Number (fromString) as DataNumber
import Data.Maybe (fromMaybe)

infixr 0 apply as <|
infixr 0 applyFlipped as |>

main :: Effect Unit
main = do
  -- bind :: (Monad m) => m a -> (a -> m b) -> m b
  -- readTextFile :: String -> Effect String
  -- log :: String -> Effect ()
  readTextFile UTF8 "accounts.txt" `bind` log

  accountsText <- readTextFile UTF8 "accounts.txt"
  transactionsText <- readTextFile UTF8 "transactions.txt"  
  
  let accounts = parseAccounts accountsText
  map log accounts
  -- let transactions = parseTransactions transactionsText

  -- let newAccounts = processTransactions accounts transactions
  -- newAccounts |> log
  -- log (show { accountNumber: "12345", balance: { amount: 100.0, currency: USD}, name: "Joe Smith"})

data Currency = USD | MXN | EUD | THB | GBP 
instance showCurrency :: Show Currency where
  show USD = "USD"
  show MXN = "MXN"
  show EUD = "EUD"
  show THB = "THB"
  show GBP = "GBP"


type Money = 
  { amount :: Number
  , currency :: Currency
  }

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

accounts = 
  [ { accountNumber: "12345", balance: { amount: 100.0, currency: USD}, name: "Joe Smith"}
  , { accountNumber: "12346", balance: { amount: 200.0, currency: USD}, name: "Joe Smyth"}
  ]

transactions = 
  [ Bill { accountNumber: "12345", amount: { amount: 300.0, currency: THB }, bucket: "Dues"  }
  , Bill { accountNumber: "12346", amount: { amount: 400.0, currency: THB }, bucket: "Dues"  }
  , Payment { accountNumber: "12345", amount: { amount: 500.0, currency: THB }, source: "Online Payment"  }
  , Payment { accountNumber: "12346", amount: { amount: 600.0, currency: THB }, source: "Online Payment"  }
  ]

parseAccounts :: String -> Array (Either String Account)
parseAccounts text =
  let
    lines = split (Pattern "\r\n") text
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
  accountNumber <- getAccountNumber fields
  balance <- getAmount fields
  name <- index 2 fields
  pure { accountNumber, balance, name}

getAccountNumber :: Array String -> Maybe AccountNumber
getAccountNumber [accountText, _, _] =
  map show <| (DataInt.fromString accountText)
getAccountNumber _ = Nothing  

getAmount :: Array String -> Maybe Money
getAmount [_, amountText, _] = do
  let amountParts = split " " amountText
  firstText <- index 0 amountParts
  currencyText <- index 1 amountParts
  amount <- DataNumber.fromString firstText
  currency <- parseCurrency currencyText  
  pure ({amount, currency})
getAmount _ = Nothing  

parseCurrency :: String -> Maybe Currency
parseCurrency "USD" = Just USD
parseCurrency "MXN" = Just MXN
parseCurrency "GBP" = Just GBP
parseCurrency "EUD" = Just EUD
parseCurrency "THB" = Just THB
parseCurrency _ = Nothing

-- parseTransactions :: String -> Array (Either String Transaction)

-- processTransactions :: Array Account -> Array Transaction -> Array Account