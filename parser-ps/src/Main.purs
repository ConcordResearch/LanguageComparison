module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.String (Pattern(..),  contains, drop, indexOf, length, take)
import Data.String.CodeUnits (fromCharArray, toCharArray, singleton)
import Data.Int (toNumber)
import Data.Array (toUnfoldable, some, zipWith)
import Data.List ((:))
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

import Control.Applicative (pure)
import Control.Plus ((<|>))

import Text.Parsing.Parser (Parser(..), ParseError(..), runParser, ParserT)
import Text.Parsing.Parser.Language (javaStyle, haskellStyle, haskellDef)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser, digit, letter, upper)
import Text.Parsing.Parser.String (string, eof, satisfy)
import Text.Parsing.Parser.Combinators (sepBy, sepBy1, sepEndBy, sepEndBy1)
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



-- type DecodeValidation e = Validation (DecodeErrors e)

-- runLine :: foreall a.  V (Array ParseError) a
-- runLine line =
--   let 
--     row :: P (List String)
--     row = (makeRow "," makeField)

--     -- runParser :: s -> Parser s a -> Either ParseError a
--     parsed :: _ -> Either ParseError a
--     parsed l = runParser l row

    
--     translate :: Either ParseError a -> V (Array ParseError) a -- List String
--     translate p =
--       either 
--         (\(Either e a) -> --Either ParseError a
--           invalid $ [e])  -- V (Array ParseError) a
--         (\(Either e a) ->
--           pure a)
--         p
    
--     validate    --pass in (List (P a)), then traverse with applicative
--   in
--     (translate (parsed line))

--Lets try and parse the account line and pass in a list


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


-- testrow :: Either ParseError (List String)
-- testrow = (runParser "1234,234 USD" (makeRow "," makeField))


-- take in the string/cursor
-- you have the mechanism to break down by newline

-- how do I traverse over a list, whats the signature
-- partially apply one part of the traverse, then 




--toField :: a -> m b

-- what does a trraverse/traverse look like

--WORKS
  -- (String -> Maybe (Array Char)) -> Array String -> Maybe (Array Char)
-- > traverse (\s -> parse s digit) ["1", "2", "3"]
-- (Right ['1','2','3'])

--   (String -> Maybe (List Char)) -> Array String -> Maybe (Array (List Char))
-- > traverse (\s -> parse s (digit `sepBy1` (string ","))) ["1,2,3"]
-- (Right [('1' : '2' : '3' : Nil)])

-- 2 contexts - 
--   1) result of parse             Maybe String
--   2) List  of strings (columns)  ["1", "2", "3"]

-- The result of parse also then needs to convert the either
-- Then the either to extract the information into a V



h :: Either ParseError (Array (V (Array ParseError) String))
h =
  let
    -- parse a line into strings, with separator ','
    fi :: P (List String)
    fi  = makeField `sepBy1` (string ",")

    -- gi s = parse s (fi `sepBy` (string "\n")) --dont do sepBy
    
    --parse a line ending in a '\n'
    hi :: P (List String)
    hi = do
      a <- fi
      _ <- string "\n"
      pure a

    -- this can be the decoder which parses each lines values
    decode :: List String -> V (Array ParseError) String
    decode ls = invalid [ParseError "Some Error"  (Position {line: 1, column: 2})] 
    -- decode ls = pure $ foldl (\acc val -> append acc val) "" ls

    -- combines the decoder and line parser
    ji :: P (List String) -> P (V (Array ParseError) String) --use this as the decoder
    ji pls = decode <$> pls

    gi :: String -> Either ParseError (V (Array ParseError) String)
    gi s = parse s (ji hi)

    -- ki = do
    --   a <- ji hi
    --   end 

    --      row parser
    file :: P (List String) -> P (List (List String))
    file r = r `sepEndBy` string "\n" <* eof

  in 
    traverse (gi) ["1,2,3\n4,5,6"]



v :: Array String
v = ["1", "2", "3"]
v' :: Array (Maybe (Array String))
v' = [Just (["1", "2", "3"]), Just (["4", "5", "6"])]

-- f :: String -> Maybe 
f s = parse s (digit `sepBy1` (string ","))
--traverse :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
fv = traverse f v

--maybe parse 123 with digit in tofield



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
