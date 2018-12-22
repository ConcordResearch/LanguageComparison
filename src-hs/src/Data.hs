{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances   #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Data where

import Prelude
import Control.Lens
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.Text (Text)
import TextShow (TextShow, showb, showt, showbList, unlinesB, fromText, singleton)
import Data.Foldable (foldl')

data Currency = USD | MXN | EUD | THB | GBP
  deriving (Eq, Ord, Generic)
instance Hashable Currency 

instance TextShow Currency where
  -- for Text
  showt USD = "USD"
  showt MXN = "MXN"
  showt EUD = "EUD"
  showt THB = "THB"
  showt GBP = "GBP"
  -- for Builder
  -- showb :: a	-> Builder
  -- fromText :: Text -> Builder
  showb = fromText . showt
  -- for List to Builder
  -- showbList :: [a]	-> Builder
  -- unlinesB :: [Builder] -> Builder  -Merges several Builders, separating them by newlines.
  showbList arr = unlinesB $ map showb arr

data Money = Money
  { _value :: Int
  , _currency :: Currency
  }
instance TextShow Money where
  showt c = (showt $ _value c) <> " " <> (showt $ _currency c) 
  showb = fromText . showt

newtype AccountNumber = AccountNumber Text
  deriving (Eq, Ord, Generic)
instance Hashable AccountNumber
instance TextShow AccountNumber where
  showt (AccountNumber n) = n
  showb = fromText . showt

data Account = Account
  { _accountNumber :: AccountNumber
  , _balance :: Money
  , _name :: Text
  }
instance TextShow Account where
  showt (Account acctNum bal name) = 
    "Account { accountNumber: " 
      <> showt acctNum
      <> ", balance: "
      <> showt bal
      <> ", name: "
      <> showt name
      <> " }"
  showb = fromText . showt
  -- showbList arr = foldl' (\acc a -> acc <> singleton '\n' <> a) mempty $ map showb arr
  showbList arr = unlinesB $ map showb arr

data Transaction
  = Bill 
    { _accountNumber :: AccountNumber
    , _amount :: Money
    , _bucket :: Text
    }
  | Payment
    { _accountNumber :: AccountNumber
    , _amount :: Money
    , _source :: Text
    }
instance TextShow Transaction where
  showt (Bill acctNum amt bucket) = 
    "Bill { accountNumber: " 
      <> showt acctNum
      <> ", amount: "
      <> showt amt
      <> ", bucket: "
      <> showt bucket
      <> " }"
  showt (Payment acctNum amt source) = 
    "Bill - accountNumber: " 
      <> showt acctNum
      <> ", amount: "
      <> showt amt
      <> ", source: "
      <> showt source
      <> " }"
  showb = fromText . showt

a1 :: Account
a1 = Account
  { _accountNumber = AccountNumber "345"
  , _balance = Money
    { _value = 25
    , _currency = USD
    }
  , _name = "Jorge"
  }

t1 :: Transaction
t1 = Bill 
  { _accountNumber = AccountNumber "123"
  , _amount = Money
    { _value = 10
    , _currency = USD
    }
  , _bucket = "Bill"
  }

t2 :: Transaction
t2 = Payment
  { _accountNumber = AccountNumber "234"
  , _amount = Money
    { _value = 15
    , _currency = USD
    }
  , _source = "Online Payment"
  }



-- -- | Example Code
-- -- Try:

-- -- > (Foo 5) ^. x
-- -- 5
-- -- > x .~ 10 $ Foo 6
-- -- Foo {_x = 10}
-- data FooBar =
--     Foo { _x :: Int}
--   | Bar { _x :: Int}
--   deriving (TextShow)

-- class HasX t where
--   x :: Lens' t Int
-- instance HasX FooBar where
--   x f (Foo x) = (\x' -> Foo x') <$> f x
--   x f (Bar x) = (\x' -> Bar x') <$> f x



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
currency :: Lens' Money Currency
currency f (Money v c) =
  (\a' -> Money { _value = v, _currency = a' } )
    <$> f c

