{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data where

import Control.Lens

data Point = Point {
    _x :: Float,
    _y :: Float
}
makeClassy ''Point

ff :: Point
ff = Point 5.0 6.1

gg :: Point
gg = set x 2.0 ff



data Currency = USD | MXN | EUD | THB | GBP
  deriving (Eq, Ord)
instance Show Currency where
  show USD = "USD"
  show MXN = "MXN"
  show EUD = "EUD"
  show THB = "THB"
  show GBP = "GBP"

data Money = Money
  { value :: Int
  , currency :: Currency
  }
makeClassy ''Money

newtype AccountNumber = AccountNumber String

data Account = Account
  { _accountNumber :: AccountNumber
  , _balance :: Money
  , _name :: String
  }
makeClassy ''Account
-- makeFields ''Account

data Transaction
  = Bill 
    { _accountNumber :: AccountNumber
    , _amount :: Money
    , _bucket :: String
    }
  | Payment
    { _accountNumber :: AccountNumber
    , _amount :: Money
    , _source :: String
    }
makePrisms ''Transaction
makeFields ''Transaction



ii :: Account -> AccountNumber
ii a = a.accountNumber 

-- hh :: Transaction -> Money
-- hh transaction =
--   case transaction of
--     Bill { amount = a } -> (_ + a.amount)
--     Payment { amount = a } -> (_ - a.amount)
