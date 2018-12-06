module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..), isJust)
import Undefined (__)
import Data.Tuple
import Data.Tuple.Nested (Tuple3(..))
import Data.Traversable (traverse)


main :: Effect Unit
main = do
  log "Hello sailor!"

plus x y = __ + y


-- runTest f args result =  

type Test e = 
  { q :: e
  , a :: e
  , name :: String
  }

test1 :: Test Int
test1 = { q: (plus 1 2), a: 3, name: "test1" }

runTest :: forall e. Eq e => Show e => Test e -> Maybe String
runTest {q, a, name} =
  if (eq q a) == false
  then Just ("Test '" <> name <> "' failed. Expected: " <> show a <> " Actual: " <> show q )
  else Nothing

tests :: forall e. Array (Test e)
tests =
  [ test1
  ]

-- traverse (\a -> if isJust a then Just a else Nothing )  [1, 1]
runTests =
  traverse runTest tests
  
