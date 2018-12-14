module FastFold
  ( fastFold
  , fastPush
  , MutableArray
  , main
  ) where

import Prelude

import Data.Array (range)
import Effect (Effect)
import Effect.Console (log)
import Time (now)

main :: Effect Unit
main = do
  testFastFold

-- | `foldr` interface which folds an array in raw
-- | JS for performance purposes, but exposes a safe
-- | interface.
-- |
-- | Timings:
-- |   160.0
testFastFold :: Effect Unit
testFastFold = do  
  -- End 
  let arr = range 1 1000000
  log "start"
  t0 <- now
  let a = fastFold (\a mutArr -> fastPush a mutArr) [] arr
  t1 <- now
  log $ "End " <> show (t1 - t0)

data MutableArray a = MutableArray a
foreign import mutableArrayBind :: forall a b. MutableArray a -> (a -> MutableArray b) -> MutableArray b
foreign import unfreeze :: forall a. Array a -> MutableArray a
foreign import freeze :: forall a. MutableArray a -> Array a
foreign import push :: forall a. a -> MutableArray a -> MutableArray a
foreign import fastFoldr :: forall a b f. (a -> b -> b) -> b -> f a -> b

fastPush :: forall a. a -> MutableArray a -> MutableArray a
fastPush = push

-- Usage:
-- fastFold (\a mutArr -> push a mutArr) [] [1,2,3]
fastFold :: forall a b. (a -> MutableArray b -> MutableArray b) -> Array b ->  Array a -> Array b
fastFold fn init arr = 
  freeze ( fastFoldr fn (unfreeze init) arr )

-- fastFoldMap :: forall b k v. (k -> v -> MutableArray b -> MutableArray b) -> Array b ->  Array (Tuple k v) -> Array b
-- fastFoldMap fn init arr = 
--   freeze ( fastFoldMap fn (unfreeze init) arr )
