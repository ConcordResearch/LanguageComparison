module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.String (Pattern(..),  contains, drop, indexOf, length, take)
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton)
import Data.Int (toNumber)
import Data.Array (toUnfoldable, some, zipWith)
import Data.List ((:), many)
import Data.List.Types (List(..))
-- import Data.List.Lazy (replicate, snoc)
-- import Data.List.Lazy.Types (List(..), Step(..),  step, nil, cons, (:))
-- import Data.List.Lazy.Types (List(..)) as LList
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Traversable (traverse, sequence, foldr)
import Data.Foldable (foldMap, all, foldl)
import Data.Unfoldable (replicateA)
import Data.Validation.Semigroup (V, invalid)
import Data.Map (Map(..), fromFoldable)
import Data.Tuple.Nested ((/\))

import Control.Applicative (pure)
import Control.Plus ((<|>))

import Text.Parsing.Parser (Parser(..), ParseError(..), runParser, ParserT)
import Text.Parsing.Parser.Language (javaStyle, haskellStyle, haskellDef)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser, digit, letter, upper)
import Text.Parsing.Parser.String (string, eof, satisfy)
import Text.Parsing.Parser.Combinators (sepBy, sepBy1, sepEndBy, sepEndBy1, manyTill, endBy)
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
-- makeRow :: String -> P String -> P (List String)
-- makeRow sep p = p `sepBy1` (string sep)

-- makeFile :: P (List (List String))
-- makeFile =
--   let
--     -- parseAccount :: P Account
--     decoder = parseAccount

--     f :: P (List String)
--     f = (makeRow "," makeField)
    
--     -- g :: P Account -> P (List String) -> P 
--     -- g = 
--   in
--     f `sepBy1` (string "\n")




-- h :: Either ParseError (Array (V (Array ParseError) String))
-- j :: forall a. String -> String -> (List String -> V (Array ParseError) a) -> P (List (V (Array ParseError) a))
j :: forall a. String -> String -> (List String -> V (Array ParseError) a) -> P (V (Array ParseError) (List a))
j seperator eol decoder' =
  let
    -- parse a line into strings, with separator ','
    fields :: P (List String)
    fields  = makeField `sepBy1` (string ",")

    --parse a line ending in a '\n'
    line :: String -> P (List String)
    line eol' = fields <* string eol'
    
    -- combines the decoder and line parser
    -- ji :: P (List String) -> P (V (Array ParseError) String) --use this as the decoder
    -- ji pls = decode <$> pls
    decodeLine :: forall a. String -> String -> (List String -> V (Array ParseError) a) -> P (V (Array ParseError) a) --use this as the decoder
    decodeLine sep eol decode = decode <$> (line eol)

    mi = (many (decodeLine seperator eol decoder')) <* eof

    -- -- this can be the decoder which parses each lines values
    -- decoder :: forall a. List String -> V (Array ParseError) a
    -- -- decode ls = invalid [ParseError "Some Error"  (Position {line: 1, column: 2})] 
    -- decoder ls = pure $ foldl (\acc val -> append acc val) "" ls

    merge :: forall a. P (List (V (Array ParseError) a)) -> P (V (Array ParseError) (List a))
    merge p = do
      i <- p
      pure $ sequence i

  in 
    merge mi

h = j "," "\n" stringDecoder

-- k = parse "123,456 USD" parseAccount

-- this can be the decoder which parses each lines values
stringDecoder :: List String -> V (Array ParseError) String
-- decode ls = invalid [ParseError "Some Error"  (Position {line: 1, column: 2})] 
stringDecoder ls = pure $ foldl (\acc val -> append acc val) "" ls




validateAccountNumber :: String -> V (Array ParseError) AccountNumber
validateAccountNumber s =  invalid $ [ParseError "Some Error"  (Position {line: 1, column: 2})]

validateMoney :: String -> V (Array ParseError) Money
validateMoney s =  invalid $ [ParseError "Some Error"  (Position {line: 1, column: 2})]

type InvalidAccount = 
  { accountNumber :: String
  , balance :: String
  }

-- accountDecoder :: List String -> V (Array ParseError) Account
-- accountDecoder ls = { accountNumber: _, balance: _ }
--   <$> validateAccountNumber acct.accountNumber
--   <*> validateMoney acct.balance

-- type Decoder = Decoder a --an applicateive
--takes a (List String) -> 

headers :: Map String String
headers =
  fromFoldable 
  [ "" /\ ""
  , "" /\ ""
  ]

type Header a =
  { name :: String
  , position :: Int
  }

--create a decoder
-- __ :: Map String _ -> Map String (Parser) -> Decoder a
-- __ headers validators =
--   let 
--     f {name, position} = 
--     -- return 'Parser Set' which has more metadata like index, parser, column header
--   in
--     f <$> headers

  -- map over the headers
  -- find validator out of headers
  -- append parser to list of parsers, can we attach more metadata?





-- Traverse with parser
  -- (String -> Maybe (Array Char)) -> Array String -> Maybe (Array Char)
-- > traverse (\s -> parse s digit) ["1", "2", "3"]
-- (Right ['1','2','3'])

--   (String -> Maybe (List Char)) -> Array String -> Maybe (Array (List Char))
-- > traverse (\s -> parse s (digit `sepBy1` (string ","))) ["1,2,3"]
-- (Right [('1' : '2' : '3' : Nil)])


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

parseResult = runParser testFile defaultParsers.file
parse = runParser
-- run = isTrue testResult parseResult
