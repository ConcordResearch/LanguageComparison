{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE NoImplicitPrelude      #-}

{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Data where

import Prelude --(Show, String, Int)
import Control.Lens

-- data Point = Point {
--     _x :: Float,
--     _y :: Float
-- }
-- makeClassy ''Point

-- ff :: Point
-- ff = Point 5.0 6.1

-- gg :: Point
-- gg = set x 2.0 ff

data Currency = USD | MXN | EUD | THB | GBP
  deriving (Eq, Ord)
instance Show Currency where
  show USD = "USD"
  show MXN = "MXN"
  show EUD = "EUD"
  show THB = "THB"
  show GBP = "GBP"

data Money = Money
  { _value :: Int
  , _currency :: Currency
  }
instance Show Money where
  show c = (show $ _value c) <> " " <> (show $ _currency c) 

newtype AccountNumber = AccountNumber String
  deriving (Show)

data Account = Account
  { _accountNumber :: AccountNumber
  , _balance :: Money
  , _name :: String
  }

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

a1 :: Account
a1 = Account
  { _accountNumber = AccountNumber "345"
  , _balance = Money
    { _value = 102
    , _currency = USD
    }
  , _name = "Jorge"
  }

t1 :: Transaction
t1 = Bill 
  { _accountNumber = AccountNumber "123"
  , _amount = Money
    { _value = 52
    , _currency = USD
    }
  , _bucket = "Bill"
  }

t2 :: Transaction
t2 = Payment
  { _accountNumber = AccountNumber "234"
  , _amount = Money
    { _value = 432
    , _currency = USD
    }
  , _source = "Online Payment"
  }



-- | Example Code
-- Try:

-- > (Foo 5) ^. x
-- 5
-- > x .~ 10 $ Foo 6
-- Foo {_x = 10}
data FooBar =
    Foo { _x :: Int}
  | Bar { _x :: Int}
  deriving (Show)

class HasX t where
  x :: Lens' t Int
instance HasX FooBar where
  x f (Foo x) = (\x' -> Foo x') <$> f x
  x f (Bar x) = (\x' -> Bar x') <$> f x



class HasAccountNumber t where
  accountNumber :: Lens' t AccountNumber
instance HasAccountNumber Transaction where
  accountNumber f (Bill an am b) =
    (\a' -> Bill { _accountNumber = a', _amount = am, _bucket = b } )
      <$> f an
  accountNumber f (Payment an am s) =
    (\a' -> Payment { _accountNumber = a', _amount = am, _source = s } )
      <$> f an
instance HasAccountNumber Account where
  accountNumber f (Account an bal n) =
    (\a' -> Account { _accountNumber = a', _balance = bal, _name = n } )
      <$> f an

--implement balance change
class HasAmount t where
  amount :: Lens' t Money
instance HasAmount Account where
  amount f (Account an bal n) =
    (\a' -> Account { _accountNumber = an, _balance = a', _name = n } )
      <$> f bal
instance HasAmount Transaction where
  amount f (Bill an am b) =
    (\a' -> Bill { _accountNumber = an, _amount = a', _bucket = b } )
      <$> f am
  amount f (Payment an am s) =
    (\a' -> Payment { _accountNumber = an, _amount = a', _source = s } )
      <$> f am

value :: Lens' Money Int
value f (Money v c) =
  (\a' -> Money { _value = a', _currency = c } )
    <$> f v

    


-- Convert from one currency to another
-- > convert (Money 100 USD) USD
-- Just 100 USD
-- > convert (Money 100 USD) THB
-- Just 200 USD
convert :: Money -> Currency -> Maybe Money
convert m c =
  if _currency m == c
  then Just m
  else do
    Just $ value %~ (\v -> v * 2) $ m


-- add or subtract a transaction amount from account amount
-- appl :: Account -> Transaction -> Transaction


-- amount :: Lens' Transaction Money
-- amount f t =
--   (\a' -> Bill { _accountNumber = _accountNumber t, _amount = a', _bucket = _bucket t } )
--     <$> f (_accountNumber t)



-- makeClassy ''Money
-- makeClassy ''Account
-- makePrisms ''Transaction
-- makeFields ''Transaction

-- makeFieldsNoPrefix ''Money
-- makeFieldsNoPrefix ''Account
-- makeFieldsNoPrefix ''Transaction


-- getAccountNumber :: Account -> AccountNumber
-- getAccountNumber acct = view accountNumber acct

-- changeAccountName :: String -> Account -> Account
-- changeAccountName n a =
--   over name (const n) a

--change the accountName



-- addOneToTransaction :: Account -> Account
-- addOneToTransaction a = over (balance . value) (+1) a

-- addMoneyToTransaction :: Money -> Transaction -> Transaction
-- addMoneyToTransaction m t =
--   over (amount) (\mm -> (view amount mm) + (view amount m)) t

-- hh :: Transaction -> Money
-- hh transaction =
--   case transaction of
--     Bill { amount = a } -> (_ + a.amount)
--     Payment { amount = a } -> (_ - a.amount)
