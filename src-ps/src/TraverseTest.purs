module TraverseTest where

import Prelude

import Data.Array (index, partition, length, snoc, filter, nub, take, range)
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
import Effect (Effect, foreachE)
import Effect.Console (log)
import Effect.Exception (throwException)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do

  --This seems to respond as expected. Maybe 7-9 sec for 1M
  -- log "Start"
  -- let i = 1000000
  -- let strings =  (range 1 i)
  -- log "Strings Created"
  -- _ <- traverse ( show >>> log ) (strings)
  -- pure unit

  -- /////////////////

  log "Read Started"
  accountsText <- readTextFile UTF8 "accounts-1m.txt"
  log "File Read Complete"
  --let accounts = take 600000 $ parseAccounts accountsText
  let accounts = take 1000000 $ split (Pattern "\n") accountsText
  log "Parse Complete"
  let stringedAccts = map (\s -> show s <> show s) accounts
  log "Stringed Complete"

  --This takes 30 seconds
  _ <- traverse ( log ) (stringedAccts)

  --this is instant
  -- foreachE stringedAccts log

  pure unit












--for Map
-- fold :: forall a z. (z -> String -> a -> z) -> z -> Object a -> z



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


-- Example of the ST monad to push values into an array.

-- Rights complete 481.0
-- Rights1 complete 1605.0
--Rights with a mutable array
-- rights1 :: forall a b. Array (Either a b) -> Array b
-- rights1 array = 
--   let 
--     inner :: forall c. ST c (Array b)
--     inner = do
--       -- Go create a new empty array
--       -- empty :: forall a. f a
--       arr <- empty
      
--       let 
--         insertIfValid e =
--           case e of
--             -- push :: forall h a. a -> STArray h a -> ST h Int
--             Right v -> push v arr *> pure unit
--             _ -> pure unit

--       -- foreach :: forall r a. Array a -> (a -> ST r Unit) -> ST r Unit
--       -- ST.foreach xs f runs the computation returned by the function f for each of the inputs xs.
--       _ <- foreach array insertIfValid --(\a -> push a arr *> pure unit )
      
--       freeze arr
--       -- newarr <- freeze arr
--       -- pure newarr
--   in
--     run inner
