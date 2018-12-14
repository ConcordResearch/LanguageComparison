module Main where

import Prelude

import Control.Monad.ST.Internal (ST, foreach)
import Control.Monad.ST.Internal (ST, foreach, run)
import Control.Monad.State (get)
import Data.Array (index, partition, length, snoc, filter, nub, take, range, cons, fromFoldable, singleton, mapMaybe)
import Data.Array.ST (STArray, empty, freeze, push, unsafeFreeze)
import Data.Either (Either(..), either, isRight, isLeft, hush)
import Data.Foldable (foldl, foldr)
import Data.Function (apply, applyFlipped)
import Data.Int (fromString) as DataInt
import Data.List.Lazy (elemLastIndex)
import Data.Map (Map) --, insert, lookup, values)
import Data.Map as Map
import Data.HashMap (HashMap, insert, lookup, values)
import Data.HashMap as HM
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
import Foreign.Object (Object, freezeST)
import Foreign.Object.ST (STObject, new, poke)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)


import FastFold (fastFold, fastPush, MutableArray)
import FastFold as FF

foreign import now :: Effect Number

infixr 0 apply as <|
infixl 0 applyFlipped as |>


main :: Effect Unit
main = do
  main1


-- Rights complete 481.0
-- Rights1 complete 1605.0
--Rights with a mutable array
rights1 :: forall a b. Array (Either a b) -> Array b
rights1 array = 
  let 
    inner :: forall c. ST c (Array b)
    inner = do
      -- Go create a new empty array
      -- empty :: forall a. f a
      arr <- empty
      
      let 
        insertIfValid e =
          case e of
            -- push :: forall h a. a -> STArray h a -> ST h Int
            Right v -> push v arr *> pure unit
            _ -> pure unit

      -- foreach :: forall r a. Array a -> (a -> ST r Unit) -> ST r Unit
      -- ST.foreach xs f runs the computation returned by the function f for each of the inputs xs.
      _ <- foreach array insertIfValid --(\a -> push a arr *> pure unit )
      
      freeze arr
  in
    run inner




-- To Look at:
-- HashMap  https://github.com/fehrenbach/purescript-unordered-collections
-- Binary search on list https://pursuit.purescript.org/packages/purescript-sorted-arrays/0.2.0


-- fastFold :: forall k v. Ord k => Show k => Array (Tuple k v) -> Object v
-- fastFold arr =
--   -- foldl (\acc record -> insert record.key record acc) Map.empty ArrayOfRecords
--   let
    
--     -- poke :: forall a r. String -> a -> STObject r a -> ST r (STObject r a)
--     f :: forall r . STObject r v -> Tuple k v -> ST r Unit
--     f map' (Tuple k v) = (poke (show k) v map' *> pure unit) 

--     inner :: forall r. ST r (Object v)
--     inner = do
      
--     -- map' :: STObject r v
--       map' <- new :: ST r (STObject r v)
      
--       -- let f map' (Tuple k v) = (poke (show k) v map' *> pure unit) 
  
--       _ <- foreach arr (f map') -- (\(k /\ v) -> poke (show k) v map' *> pure unit )
      
--       --freezeST :: forall a r. STObject r a -> ST r (Object a)

--       freezeST map'
--   in
--     run inner





-- class UpdateAsMutable a where
--   get :: 
--   set :: 
--   freeze :: STArray b -> Array b
--   unfreeze :: Array b -> STArray b
  
--   freeze :: STObject b -> Object b
--   unfreeze :: Object b -> STObject b

--   freeze :: ?? -> Map k v
--   unfreeze :: CMap k v -> STCMap r k v

--   var map = new CMap();
--   map.beginMutation(); :: CMap k v -> STCMap r k v
--   map.insert(a, b);
--   map.insert(a, b);
--   map.insert(a, b);
--   map.endMutation();



-- fastFold' :: (b -> a -> b) -> Array b -> Foldable a -> b
-- create STArray
-- copy contents of Array b -> STArray
-- fold
-- freeze
-- return final value


-- fastUpdate :: Array a -> (MutableArray a -> MutableArray a) -> Array a
-- fastUpdate arr fn = do
--   mut = unfreeze arr
--   fn mut
--   freeze mut

-- foldFn :: (MutableArray a -> b -> MutableArray a)
-- foldFn mutArr value = mutatingPush value mutArr

-- fastUpdate regularArray (\mutArray -> foldl foldFn empty mutArray)


-- exports.fastPush = (array, item) => array.push(item);
-- exports.fastLookup = (map, key) => map[key] ?? Maybe.Nothing()
-- exports.fastInsert = (map, key, value) => map[key] = value

-- function fold(fn, start, array){
--   let accum = start;
--   for(var val of array) {
--     accum = fn(accum, val);
--   }
-- }


-- "Create Account Lookup complete 16265.0" with 1M accounts

createAccountLookup1 :: Array Account -> Map AccountNumber Account
createAccountLookup1 arr = 
  Map.fromFoldable $ map (\account -> Tuple account.accountNumber account) arr

-- createAccountLookup2 :: Array Account -> Object Account
-- createAccountLookup2 arr = 
  -- fastFold $ map (\account -> Tuple account.accountNumber account) arr
  
createAccountLookup3 :: Array Account -> HashMap AccountNumber Account
createAccountLookup3 arr = 
  HM.fromFoldable $ map (\account -> Tuple account.accountNumber account) arr






-- __ :: Array a -> (a -> Tuple k a) -> Array (Tuple k a)

-- fromFoldable $ map (\a -> Tuple a.key a) 

-- Map
-- fromFoldable :: forall f k v. Ord k => Foldable f => f (Tuple k v) -> Map k v








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


main4 :: Effect Unit
main4 = do  
  -- End 160.0
  let arr = range 1 1000000
  log "start"
  t0 <- now
  let a = fastFold (\a mutArr -> fastPush a mutArr) [] arr
  t1 <- now
  log $ "End " <> show (t1 - t0)


main3 :: Effect Unit
main3 = do  

  let 
    inner :: forall r. Array Int -> ST r (Object Int)
    inner arr = do
      
      map' <- new :: ST r (STObject r Int)
      
      _ <- foreach arr (\i -> poke (show i) (i * 10) map' *> pure unit)  -- (\(k /\ v) -> poke (show k) v map' *> pure unit )
      
      freezeST map'

  let arr = range 1 1000000
  log "start"
  t0 <- now
  let a = run (inner arr)
  t1 <- now
  log $ "End " <> show (t1 - t0)



main2 :: Effect Unit
main2 = do  
  
  tt0 <- now
  accountsText <- readTextFile UTF8 "accounts-1m.txt"
  tt1 <- now
  log <| "Read Accounts Complete " <> (show <| tt1 - tt0)
  
  let accounts = parseAccounts accountsText
  tt2 <- now
  log <| "Parse Accounts complete " <> (show <| tt2 - tt1)
  
  -- This was REALLY slow cause of `snoc` in the rights/lefts
  -- let accountErrors = lefts accounts
 
  let validAccounts = rights accounts
  log <| "ValidAccounts " <> show (length validAccounts)

  tt3 <- now
  log <| "Rights complete " <> (show <| tt3 - tt2)

  let validAccounts1 = rights1 accounts
  log <| "ValidAccounts1 " <> show (length validAccounts1)

  tt4 <- now
  log <| "Rights1 complete " <> (show <| tt4 - tt3)

  -- createAccountLookup is slow because of `insert`

  -- 15330.0
  let accountLookup = createAccountLookup validAccounts
  tt5 <- now
  log <| "Create Account Lookup complete " <> (show <| tt5 - tt4)

  -- 16482.0
  let tupledAccounts = map (\account -> Tuple account.accountNumber account) validAccounts
  tt6 <- now
  log <| "Create tupledAccounts complete " <> (show <| tt6 - tt5)

  -- let accountLookup2 = fastFold tupledAccounts
  -- tt7 <- now
  -- log <| "Create Account Lookup 2 complete " <> (show <| tt7 - tt6)

  -- 2857.0
  let accountLookup = createAccountLookup3 validAccounts
  tt7 <- now
  log <| "Create Account Lookup 3 complete " <> (show <| tt7 - tt6)



--  To run:
--    $ node -e "require('./output/Main').main()"

main1 :: Effect Unit
main1 = do

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
  
  let accountLookup = createAccountLookup3 validAccounts
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

  let processedValues = fromFoldable $ values accountMap
  tt8 <- now
  log <| "Values ToArray Complete " <> (show <| tt8 - tt7)

  --the use of `traverse` was very slow. using foreachE solves it
  --_ <- traverse ( show >>> log ) (values accountMap)  
  foreachE (processedValues) ( show >>> log )
  
  tt9 <- now
  
  -- Before HashMap change - on GB laptop
  -- Read Accounts Complete 47.0
  -- Read Transactions Complete 72.0
  -- Parse Accounts complete 3569.0
  -- Accounts Right Complete 145.0
  -- Create Account Lookup complete 10079.0
  -- Parse Transaction Complete 4404.0
  -- Process Transactions Complete 37111.0
  -- Complete 42447.0
  -- Total 97874.0
  
  -- After HashMap change - on GB laptop
  -- Read Accounts Complete 49.0
  -- Read Transactions Complete 61.0
  -- Parse Accounts complete 3731.0
  -- Accounts Right Complete 152.0
  -- Create Account Lookup complete 2953.0
  -- Parse Transaction Complete 4730.0
  -- Process Transactions Complete 27945.0
  -- Values ToArray Complete 1017.0
  -- Complete 31522.0
  -- Total 72160.0

  
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
  foldl (\acc acct -> Map.insert acct.accountNumber acct acc) Map.empty accounts


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


