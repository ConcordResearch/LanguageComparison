module ExamplesST where

import Prelude
import Control.Monad.ST.Internal (ST, foreach, run)
import Data.Array (foldl, range)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Foreign.Object
import Foreign.Object.ST (STObject, new, poke)
import Foreign.Object.ST.Unsafe (unsafeFreeze)
import Data.HashMap as HM
import Data.Map as Map
import Time (now) -- this is a local module

testMap :: Effect Unit
testMap = do
  -- 
  let arr = range 1 1000000
  log "Start Map"
  t0 <- now
  
  let a = foldl (\acc i -> Map.insert (show i) (i * 10) acc) Map.empty arr

  t1 <- now
  log $ "End Map" <> show (t1 - t0)
  log ""


-- | This example takes an Array Int and pushes the values
-- | into the `Object` type, which is a wrapper over the raw
-- | JS object.
test :: Effect Unit
test = do
  let 
    inner :: forall r. Array Int -> ST r (Object Int)
    inner arr = do
      
      map' <- new :: ST r (STObject r Int)
      
      _ <- foreach arr \i -> do
        _ <- poke (show i) (i * 10) map'
        pure unit
      
      unsafeFreeze map'

  let arr = range 1 1000000
  log "Test ST - Start"
  t0 <- now
  let a = run (inner arr)
  t1 <- now
  log $ "Test ST - End. Time: " <> show (t1 - t0)
  
-- | There are code changes that can significantly impact
-- | the performance of the `ST`/`Object` implementation:
-- | 1) using `unsafeFreeze` when returning an object 
-- |  doesn't copy the object. This appears to have a
-- |  decent impact on performance.
-- | 2) using a `do` instead of `*>` when returning
-- |  utilizes more of the compiler optimizations around 
-- |  `do`. This seems to have a significant improvement
-- |
-- | Timings:
-- |    727.0 w/ unsafeFreeze and do
-- |    984.0 w/ freeze and do
-- |    1298.0 w/ unsafeFreeze and NOT do
-- |    1574.0 w/ freeze and NOT do
testSTOpt :: Effect Unit
testSTOpt = do
  let 
    inner :: forall r. Array Int -> ST r (Object Int)
    inner arr = do
      
      map' <- new :: ST r (STObject r Int)
      
      -- | Using `do` here has a significant impact over using `*>`
      -- _ <- foreach arr \i -> do
      --   _ <- poke (show i) (i * 10) map'
      --   pure unit

      -- | This is the slower example using `*>`
      _ <- foreach arr (\i -> poke (show i) (i * 10) map' *> pure unit)


      -- | Because `unsafeFreeze` doesn't make a copy of the 
      -- | object, there is a perf improvement
      -- unsafeFreeze map'

      -- | `freezeST` will make a copy of the object, which may 
      -- | be needed in some situations.
      Foreign.Object.freezeST map'

  let arr = range 1 1000000
  log "ST Perf Start"
  t0 <- now
  let a = run (inner arr)
  t1 <- now
  log $ "ST Perf End " <> show (t1 - t0)
  log ""


-- | The usage of HashMap is slower than the `Object` and `ST`
-- | implementation, but the HashMap implementation is much simpler
-- | and much faster than the `Map` implementation
testHashMap :: Effect Unit
testHashMap = do
  -- 2936.0 w/ HashMap
  let arr = range 1 1000000
  log "start hash"
  t0 <- now
  
  let a = foldl (\acc i -> HM.insert (show i) (i * 10) acc) HM.empty arr

  t1 <- now
  log $ "End hash" <> show (t1 - t0)
  log ""

