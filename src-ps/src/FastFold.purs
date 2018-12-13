module FastFold where

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
import Foreign.Object (Object, freezeST)
import Foreign.Object.ST (STObject, new, poke)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- foreign import now :: Effect Number

main :: Effect Unit
main = do  

  test


test :: Effect Unit
test = do
  let 
    inner :: forall r. Array Int -> ST r (Object Int)
    inner arr = do
      
      map' <- new :: ST r (STObject r Int)
      
      _ <- foreach arr (\i -> poke (show i) (i * 10) map' *> pure unit)  -- (\(k /\ v) -> poke (show k) v map' *> pure unit )
      
      freezeST map'

  -- let arr = range 1 1000000
  -- log "start"
  -- t0 <- now
  -- let a = run (inner arr)
  -- t1 <- now
  -- log $ "End " <> show (t1 - t0)
  log ""




fastFold :: forall k v. Ord k => Show k => Array (Tuple k v) -> Object v
fastFold arr =
  -- foldl (\acc record -> insert record.key record acc) Map.empty ArrayOfRecords
  let
    
    -- poke :: forall a r. String -> a -> STObject r a -> ST r (STObject r a)
    f :: forall r . STObject r v -> Tuple k v -> ST r Unit
    f map' (Tuple k v) = (poke (show k) v map' *> pure unit) 

    inner :: forall r. ST r (Object v)
    inner = do
      
    -- map' :: STObject r v
      map' <- new :: ST r (STObject r v)
      
      -- let f map' (Tuple k v) = (poke (show k) v map' *> pure unit) 
  
      _ <- foreach arr (f map') -- (\(k /\ v) -> poke (show k) v map' *> pure unit )
      
      --freezeST :: forall a r. STObject r a -> ST r (Object a)

      freezeST map'
  in
    run inner


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

-- createAccountLookup1 :: Array Account -> Map AccountNumber Account
-- createAccountLookup1 arr = 
--   Map.fromFoldable $ map (\account -> Tuple account.accountNumber account) arr

-- createAccountLookup2 :: Array Account -> Object Account
-- createAccountLookup2 arr = 
--   fastFold $ map (\account -> Tuple account.accountNumber account) arr

