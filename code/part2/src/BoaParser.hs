-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char
-- add any other other imports you need

type Parser a = ReadP a 
type ParseError = String -- you may replace this

reserved :: [String]
reserved = [ "None", "True", "False", "for", "if", "in", "not"]

whitespace :: Parser ()
whitespace = do
  -- skipMany (satisfy isSpace);
  munch (isSpace);
  return ()

atLeastWhitespace :: Parser ()
atLeastWhitespace = do
  munch1 isSpace;
  return ()
  

comment :: Parser ()
comment = do
  symbol "#";
  munch (/= '\n');
  return ()
  <|>
  do
  symbol "#";
  munch (/= '\n');
  eof
  <|>
  return ()

lexeme :: Parser a -> Parser a
lexeme p = do
  whitespace
  a <- p;
  whitespace;
  comment;
  return a

symbol :: String -> Parser ()
symbol s = lexeme $ do
  string s;
  return ()

pProgram :: Parser Program
pProgram = do
  comment;
  pStmts
  
pStmts :: Parser [Stmt]
pStmts = do
  stmt <- pStmt;
  symbol ";";
  stmts <- pStmts;
  return (stmt:stmts)
  <|>
  do
  stmt <- pStmt;
  return [stmt]

pStmt :: Parser Stmt
pStmt = do
  var <- ident;
  symbol "=";
  SDef var <$> pExpr
  <|>
  do
  SExp <$> pExpr

pExpr :: Parser Exp
pExpr = lexeme $ do
  symbol "not";
  atLeastWhitespace;
  Not <$> pExpr
  <|>
  do
  pExpr'

pExpr' :: Parser Exp
pExpr' = do
  term <- pComplexTerm;
  pOper1 term;

pComplexTerm :: Parser Exp 
pComplexTerm = lexeme $ do
  between (symbol "(") (symbol ")") pExpr;
  <|>
  do
  var <- ident;
  symbol "(";
  exprz <- pExprz;
  symbol ")";
  return (Call var exprz)
  <|>
  do
  symbol "[";
  exprz <- pExprz;
  symbol "]";
  return (List exprz)
  <|>
  do
  symbol "[";
  expr <- pExpr;
  forC <- pForClause;
  clauses <- pClausez;
  symbol "]";
  return (Compr expr (forC:clauses))
  <|>
  pTerminal

pTerminal :: Parser Exp
pTerminal = do
    Const <$> (IntVal <$> pNum)
    <|>
    do
    symbol "'"
    Const <$> (StringVal <$> pString)
    <|>
    do
    Var <$> ident
    <|>
    do
    Const <$> pBool

pOper1 :: Exp -> Parser Exp
pOper1 exp = lexeme $ do
    symbol "==";
    expr <- pExpr';
    pOper2 (Oper Eq exp expr)
    <|>
    do
    symbol "!=";
    expr <- pExpr';
    newExpr <- pOper2 (Oper Eq exp expr);
    return (Not newExpr)
    <|>
    do
    symbol "<="
    expr <- pExpr';
    newExpr <- pOper2 (Oper Less exp expr);
    return (Not newExpr)
    <|>
    do
    symbol "<";
    expr <- pExpr';
    pOper2 (Oper Less exp expr)
    <|>
    do
    symbol "<="
    expr <- pExpr';
    newExpr <- pOper2 (Oper Greater exp expr);
    return (Not newExpr)
    <|>
    do
    symbol ">";
    expr <- pExpr';
    pOper2 (Oper Greater exp expr)
    <|>
    do
    symbol ">="
    expr <- pExpr';
    newExpr <- pOper2 (Oper Less exp expr);
    return (Not newExpr)
    <|>
    do
    atLeastWhitespace
    symbol "in";
    atLeastWhitespace;
    expr <- pExpr';
    pOper2 (Oper In exp expr)
    <|>
    do
    atLeastWhitespace
    symbol "not"
    atLeastWhitespace
    symbol "in"
    atLeastWhitespace
    expr <- pExpr;
    newExpr <- pOper2 (Oper In exp expr);
    return (Not newExpr)
    <|>
    pOper2 exp
    

pOper2 :: Exp -> Parser Exp 
pOper2 exp = do
    symbol "+"
    Oper Plus exp <$> pExpr'
    <|>
    do
    symbol "-"
    Oper Minus exp <$> pExpr'
    <|>
    pOper3 exp
    
pOper3 :: Exp -> Parser Exp
pOper3 exp = do
    symbol "*"
    Oper Times exp <$> pExpr'
    <|>
    do
    symbol "//"
    Oper Div exp <$> pExpr'
    <|>
    do
    symbol "%"
    Oper Mod exp <$> pExpr'
    <|>
    do
    return exp

pExprz :: Parser [Exp]
pExprz = do
    pExprs
    <|>
    return mempty

pExprs :: Parser [Exp]
pExprs = do
    expr <- pExpr
    symbol ","
    exprs <- pExprs
    return (expr:exprs)
    <|>
    do
    expr <- pExpr
    return [expr]

pForClause :: Parser CClause 
pForClause = do
    atLeastWhitespace
    symbol "for"
    atLeastWhitespace
    var <- ident
    atLeastWhitespace
    symbol "in"
    atLeastWhitespace
    CCFor var <$> pExpr

pIfClause :: Parser CClause
pIfClause = do
    symbol "if"
    atLeastWhitespace
    CCIf <$> pExpr

pClausez :: Parser [CClause]
pClausez = do
    clause <- pForClause
    clauses <- pClausez
    return (clause:clauses)
    <|>
    do
    clause <- pIfClause
    clauses <- pClausez
    return (clause:clauses)
    <|>
    return mempty

pBool :: Parser Value
pBool = lexeme $ do
    string "True"
    return TrueVal
    <|>
    do
    string "False"
    return FalseVal
    <|>
    do
    string "None"
    return NoneVal

pNum :: Parser Int
pNum = lexeme $ do
  char '-';
  d <- satisfy isDigit;
  ds <- many (satisfy isDigit)
  if (read [d]::Int) == 0 then
      if null ds then return 0
      else pfail
    else return $ negate (read (d:ds))
  <|>
  do
  d <- satisfy isDigit;
  ds <- many (satisfy isDigit)
  if (read [d]::Int) == 0 && not (null ds) then pfail else return $ read (d:ds)

ident :: Parser VName
ident = lexeme $ do
  d <- satisfy isAlpha <|> char '_';
  ds <- many (satisfy isAlpha <|> char '_')
  if (d:ds) `elem` reserved then pfail
    else return (d:ds)

pString :: Parser String
pString = lexeme $ do
  string <- many (satisfy isPrint)
  let output = pString' string
  return output

pString' :: String -> String
pString' str = do
  if head str == '\''
      then []
      else head str : pString' (tail str)

parseString :: String -> Either ParseError Program
parseString s = do
  case readP_to_S pProgram s of
    [] -> Left "Nothing to parse"
    x -> case last x of
      (a, "") -> Right a
      (_, _) -> Left "Garbage left at end of input"