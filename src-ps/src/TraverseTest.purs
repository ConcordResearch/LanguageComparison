module TraverseTest where

import Prelude

import Data.Array (take, range)
import Data.String (Pattern(..))
import Data.String.Common (split)
import Data.Traversable (traverse)
import Effect (Effect, foreachE)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

main :: Effect Unit
main = do
  testTraverse

testTraverse :: Effect Unit
testTraverse = do
  --This seems to respond as expected. Maybe 7-9 sec for 1M
  log "Start"
  let i = 1000000
  let strings =  (range 1 i)
  log "Strings Created"
  _ <- traverse ( show >>> log ) (strings)
  pure unit

testForEachE :: Effect Unit
testForEachE = do
  log "Read Started"
  accountsText <- readTextFile UTF8 "accounts-1m.txt"
  log "File Read Complete"
  let accounts = take 1000000 $ split (Pattern "\n") accountsText
  log "Parse Complete"
  let stringedAccts = map (\s -> show s <> show s) accounts
  log "Stringed Complete"

  --This takes 30 seconds
  -- _ <- traverse ( log ) (stringedAccts)

  --this is instant
  foreachE stringedAccts log

  pure unit
