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
