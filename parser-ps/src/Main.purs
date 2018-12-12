module Main where

import Prelude

import Control.Applicative (pure)
import Control.Plus ((<|>))
import Data.Array (toUnfoldable, some, zipWith, snoc)
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, all, foldl)
import Data.Int (toNumber)
import Data.List ((:), many)
import Data.List.Types (List(..))
import Data.Map (Map(..), fromFoldable, lookup, insert)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), contains, drop, indexOf, length, take)
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton)
import Data.Traversable (traverse, sequence, foldr)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (replicateA)
import Data.Validation.Semigroup (V, invalid)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.CSV (defaultParsers, makeParsers)
import Text.Parsing.Parser (Parser(..), ParseError(..), runParser, ParserT)
import Text.Parsing.Parser.Combinators (sepBy, sepBy1, sepEndBy, sepEndBy1, manyTill, endBy)
import Text.Parsing.Parser.Language (javaStyle, haskellStyle, haskellDef)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (string, eof, satisfy)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser, digit, letter, upper)

main :: Effect Unit
main = do
  log "Hello sailor!"

-- https://github.com/nwolverson/purescript-csv/blob/master/src/Text/Parsing/CSV.purs#L63

type P a = Parser String a

data ParserType
  = ParseMoney (P Money)
  | ParseString (P String)

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

count :: forall s m a. Monad m => Int -> ParserT s m a -> ParserT s m (List a)
count = replicateA

isTrue exp = either (\_-> false) ((==) (exp :: List (List String)))

-- testFile = "a,,c,\n,1,2,3\n\"x\",\"field,quoted\",z\n" :: String
-- testResult = toUnfoldable $ toUnfoldable <$> [["a", "", "c", ""], ["", "1", "2", "3"], ["x", "field,quoted", "z"], [""]]
testFile = "col1,col2,col3\n123456,234 USD,Joe Smith\n234567,345 MXN,Doe Simth\n345678,456 THB,Phil Mac" :: String
-- testResult = toUnfoldable $ toUnfoldable <$> [["a", "", "c", ""], ["", "1", "2", "3"], ["x", "field,quoted", "z"], [""]]

charList = ('a' : 'b' : 'c' : 'd' : Nil)

fromCharList :: List Char -> String
fromCharList = foldr (\c a -> singleton c <> a) ""

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
headerParsers
  :: forall a. Array String
  -> Map String ParserType
  -> V (Array ParseError) (Array ParserType)
headerParsers headers validators =
  let

    lookupParser :: String -> Maybe ParserType
    lookupParser hdrName = lookup hdrName validators

    -- insert' :: Array ParserType -> ParserType -> Array ParserType
    -- insert' acc v = acc `snoc` v

    h :: V (Array ParseError) (Array ParserType) -> String -> V (Array ParseError) (Array ParserType) 
    h acc hdrName = 
      handleError hdrName $ lookupParser hdrName >>= (\v -> ((<>) acc)  <$> v)

    handleError 
      :: forall a. 
         String 
      -> Maybe (V (Array ParseError) (Array ParserType))
      -> V (Array ParseError) (Array ParserType)
    handleError hdrName m =
      maybe (error hdrName) identity m
    
    error :: String -> V (Array ParseError) (Array ParserType)
    error hdrName = invalid $ pure $ ParseError ("Expected column name: " <> hdrName)  (Position {line: 0, column: 0})

  in
    foldl h mempty headers


strs = ["1234", "234 USD"]

parsers = [string "", string "234 USD"]

mm = zipWith (\a b -> parse a b) strs parsers


  -- map over the headers
  -- find validator out of headers
  -- append parser to list of parsers, can we attach more metadata?

-- 1) take the header labels+indexes and a map that has ('HeaderLabel', ColumnParser) 
-- which create a decoder (Validator Errors (List DecodedValues)) 
-- 2)  a fxn that takes the list of column values and runs the decoder against it


-- gregberns [10:49 PM]
-- Parser question: If I need to parse a string “1234,234 USD” and have two parsers 
-- `AccountNumber`, `Money`, how do I create build a type signature `Array (Parser a)`
--    so that `a` can change type? The purpose is to create a mapping of parsers that 
--    is dynamic (the order and composition of parsers can change based on a database 
--    lookup) and can be run against a CSV file where the order of columns (things to 
--    be parsed) may change. Any thoughts on where to even start?
-- Another way to put it: 


-- Question on parsers. How can a set of parsers be formed so that `["1234", "234 USD"]` and `[Parser AccountNumber, Parser Money]` can be zipped together, assuming the list of parsers are unknown at compile time. Is this possible since the output types in the parser array dont match? Is there another data structure I can use? 



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
