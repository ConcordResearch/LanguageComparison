module Main where

import Prelude -- (Monad(..))
import Effect (Effect)
import Effect.Console (log)

import Data.String (Pattern(..),  contains, drop, indexOf, length, take)
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton)
import Data.Int (toNumber)
import Data.Array (toUnfoldable, some)
import Data.List ((:))
import Data.List.Types (List(..))
-- import Data.List.Lazy (replicate, snoc)
-- import Data.List.Lazy.Types (List(..), Step(..),  step, nil, cons, (:))
-- import Data.List.Lazy.Types (List(..)) as LList
import Data.Either (either)
import Data.Traversable (traverse, sequence, foldr)
import Data.Foldable (foldMap, all)
import Data.Unfoldable (replicateA)
import Data.Validation.Semigroup (V, invalid)

import Control.Plus ((<|>))

import Text.Parsing.Parser (Parser(..), ParseError(..), runParser, ParserT)
import Text.Parsing.Parser.Language (javaStyle, haskellStyle, haskellDef)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser, digit, letter, upper)
import Text.Parsing.Parser.String (string, satisfy)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.Pos (Position(..))

import Text.Parsing.CSV ( defaultParsers, makeParsers) --, Parsers, P, makeQuoted, makeChars, makeQchars, makeField, makeFile, , makeFileHeaded)

main :: Effect Unit
main = do
  log "Hello sailor!"


-- https://github.com/nwolverson/purescript-csv/blob/master/src/Text/Parsing/CSV.purs#L63


count :: forall s m a. Monad m => Int -> ParserT s m a -> ParserT s m (List a)
count = replicateA
-- count n p = 
--   if n <= 0 
--   then pure nil
--   else sequence (replicate n p)


isTrue exp = either (\_-> false) ((==) (exp :: List (List String)))

-- testFile = "a,,c,\n,1,2,3\n\"x\",\"field,quoted\",z\n" :: String
-- testResult = toUnfoldable $ toUnfoldable <$> [["a", "", "c", ""], ["", "1", "2", "3"], ["x", "field,quoted", "z"], [""]]

testFile = "col1,col2,col3\n123456,234 USD,Joe Smith\n234567,345 MXN,Doe Simth\n345678,456 THB,Phil Mac" :: String
-- testResult = toUnfoldable $ toUnfoldable <$> [["a", "", "c", ""], ["", "1", "2", "3"], ["x", "field,quoted", "z"], [""]]

-- toString :: List Char -> String
-- toString cs = foldr (flip snoc) "" cs

charList = ('a' : 'b' : 'c' : 'd' : Nil)

fromCharList :: List Char -> String
fromCharList = foldr (\c a -> singleton c <> a) ""

p :: TokenParser
p = makeTokenParser haskellDef

parseInt :: P Int
parseInt = p.integer

parseDigit :: P Char
parseDigit = digit

parseCurrency :: P String
parseCurrency = (<$>) fromCharList (count 3 (letter <|> upper)) 

--doesnt handle negatives and decimals properly
parseAmount :: P Number
parseAmount = (toNumber <$> p.integer) <|> p.float

data Currency = USD | MXN | EUD | THB | GBP 
instance showCurrency :: Show Currency where
  show USD = "USD"
  show MXN = "MXN"
  show EUD = "EUD"
  show THB = "THB"
  show GBP = "GBP"

type Money = 
  { amount :: Number
  , currency :: String
  }

type AccountNumber = String

type Account =
  { accountNumber :: AccountNumber
  , balance :: Money
  -- , name :: String
  }

-- > parse "100 USD" parseMoney
-- (Right { amount: 100.0, currency: "USD" })
parseMoney :: P Money
parseMoney = do
  amount <- parseAmount
  currency <- parseCurrency
  pure { amount, currency }

parseAccountNumber :: P String
parseAccountNumber = show <$> p.integer

-- > parse "123,456 USD" parseAccount
-- (Right { accountNumber: "123", balance: { amount: 456.0, currency: "USD" } })
-- parseAccount :: P Account
-- parseAccount = do
--   accountNumber <- parseAccountNumber
--   _ <- string ","
--   balance <- parseMoney
--   pure { accountNumber, balance }

parseAccount :: P Account
parseAccount = do
  accountNumber <- parseAccountNumber
  _ <- string ","
  balance <- parseMoney
  pure { accountNumber, balance }


--  entity creator -> list of Tuple(colName, parser) -> list of string -> Either (List ParseError) Entity


--need to have a methods that takes a `row` and returns 
testRow = "1234,234 USD\n2345,345 USD"




type P a = Parser String a

-- type Parsers a =
--   {
--     quoted :: (P a -> P a),
--     chars :: P String,
--     qchars :: P String,
--     field :: P String,
--     row :: P (List String),
--     file :: P (List (List String)),
--     fileHeaded :: P (List (M.Map String String))
--   }


makeChars :: String -> P String
makeChars xs = do
  fromCharArray <$> some char
  where
    char = satisfy $ excluded xs
    excluded ys = \x -> all identity $ terms ys <*> [x]
    terms ys = map (/=) $ toCharArray ys


makeField :: P String
makeField = 
  makeChars $ "," <> "\n"

-- parse testRow $ makeRow "," makeField
makeRow :: String -> P String -> P (List String)
makeRow sep p = p `sepBy1` (string sep)

makeFile :: P (List (List String))
makeFile =
  let
    -- parseAccount :: P Account
    decoder = parseAccount

    f :: P (List String)
    f = (makeRow "," makeField)
    
    -- g :: P Account -> P (List String) -> P 
    -- g = 
  in
    f `sepBy1` (string "\n")

-- do validation
--parse a row, then check results, if failed create a Error validation, Else attach the correct result

--handle errors



-- runParser ::s -> Parser s a -> Either ParseError a

runLine l =
  let 
    f :: P (List String)
    f = (makeRow "," makeField)

    parsed :: -> Either ParseError a
    parsed = runParser l (makeRow "," makeField)

    
    translate :: Either ParseError a -> 
    translate p =
      either 
        (\(Either e a) -> --Either ParseError a
          invalid $ [e])
        (\r ->
          )
        p
    
  in



--Example of Using Validation

-- validateAccountNumber :: String -> V (Array ParseError) AccountNumber
-- validateAccountNumber s =  invalid $ [ParseError "Some Error"  (Position {line: 1, column: 2})]

-- validateMoney :: String -> V (Array ParseError) Money
-- validateMoney s =  invalid $ [ParseError "Some Error"  (Position {line: 1, column: 2})]

-- type InvalidAccount = 
--   { accountNumber :: String
--   , balance :: String
--   }

-- validate :: InvalidAccount -> V (Array ParseError) Account
-- validate acct = { accountNumber: _, balance: _ }
--   <$> validateAccountNumber acct.accountNumber
--   <*> validateMoney acct.balance
  



headerColumns = 1
separator = ","
newParser = makeParsers '"' "," "\n"


parseResult = runParser testFile defaultParsers.file
parse = runParser
-- run = isTrue testResult parseResult
