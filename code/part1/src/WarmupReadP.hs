module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E ::= T E' | "-" T E'
--   E' ::= "+" T E' | "-" T E' | empty
--   T ::= num | "(" E ")"

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

whitespace :: Parser ()
whitespace = do
  many (satisfy isSpace);
  return ()

lexeme :: Parser a -> Parser a
lexeme p = do
  a <- p;
  whitespace;
  return a

symbol :: String -> Parser ()
symbol s = lexeme $ do
  string s;
  return ()

pE :: Parser Exp
pE = do
  symbol "-";
  e <- pT;
  pE' (Negate e)
  <|>
  do
  e <- pT;
  pE' e

pE' :: Exp -> Parser Exp
pE' e = do
  symbol "+";
  term <- pT;
  pE' (Add e term)
  <|>
  do
  symbol "-";
  term <- pT;
  pE' (Add e (Negate term))
  <|>
  return e

pT :: Parser Exp
pT = do
  Num <$> pNum;
  <|>
  do
  symbol "(";
  e <- pE;
  symbol ")";
  return e

pNum :: Parser Int
pNum = do
  d <- satisfy isDigit;
  ds <- many (satisfy isDigit)
  return $ read (d:ds)

parseString :: String -> Either ParseError Exp
parseString s = do
  case readP_to_S pE s of
    [] -> Left "Nothing to parse"
    x -> case last x of
      (a, "") -> Right a
      (_, _) -> Left "Garbage left at end of input"