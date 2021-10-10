-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?= 
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "var test" $
    parseString "eee + eee" @?=
      Right [SExp (Oper Plus (Var "eee") (Var "eee"))],
  testCase "left associate minus" $
    parseString "5-2*3" @?=
      Right [SExp (Oper Minus (Const (IntVal 5)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3))))],
  testCase "left associate times" $
    parseString "5*2-3" @?=
      Right [SExp (Oper Times (Const (IntVal 5)) (Oper Minus (Const (IntVal 2)) (Const (IntVal 3))))],
  testCase "left associate div" $
    parseString "5//2-3" @?=
      Right [SExp (Oper Div (Const (IntVal 5)) (Oper Minus (Const (IntVal 2)) (Const (IntVal 3))))],
  testCase "left associate mod" $
    parseString "5%2-3" @?=
      Right [SExp (Oper Mod (Const (IntVal 5)) (Oper Minus (Const (IntVal 2)) (Const (IntVal 3))))],
  testCase "non associate order" $
    parseString "5 < 5 < 5" @?=
      Left "Garbage left at end of input",
  testCase "non associate order" $
    parseString "5 > 5 > 5" @?=
      Left "Garbage left at end of input",
  testCase "non associate eq" $
    parseString "5 == 5 == 5" @?=
      Left "Garbage left at end of input",
  testCase "non associate noteq" $
    parseString "5 != 5 != 5" @?=
      Left "Garbage left at end of input",
  testCase "simple noteq" $
    parseString "5 != 5" @?=
      Right [SExp (Not (Oper Eq (Const (IntVal 5)) (Const (IntVal 5))))],
  testCase "non associate ordereq" $
    parseString "5 >= 5 >= 5" @?=
      Left "Garbage left at end of input",
  testCase "non associate ordereq" $
    parseString "5 <= 5 <= 5" @?=
      Left "Garbage left at end of input",
  testCase "paranthesis test" $
    parseString "( 2 )" @?=
      Right [SExp (Const (IntVal 2))],
  testCase "multiple stmts" $
    parseString "2; 2+2" @?=
      Right [SExp (Const (IntVal 2)), SExp (Oper Plus (Const (IntVal 2)) (Const (IntVal 2)))],
  testCase "assign value to var" $
    parseString "a = 4" @?=
      Right [SDef "a" (Const (IntVal 4))],
  testCase "not in test" $
    parseString "x not in 5" @?=
      Right [SExp (Not (Oper In (Var "x") (Const(IntVal 5))))],
  testCase "in test" $
    parseString "x in 5" @?=
      Right [SExp (Oper In (Var "x") (Const (IntVal 5)))],
  testCase "boolean True test" $
    parseString "x == True" @?=
      Right [SExp (Oper Eq (Var "x") (Const TrueVal))],
  testCase "boolean False test" $
    parseString "x == False" @?=
      Right [SExp (Oper Eq (Var "x") (Const FalseVal))],
  testCase "boolean None test" $
    parseString "x == None" @?=
      Right [SExp (Oper Eq (Var "x") (Const NoneVal))],
  testCase "list expressions" $
    parseString "[5+5 ,5-5]" @?=
      Right [SExp (List [Oper Plus (Const (IntVal 5)) (Const (IntVal 5)),Oper Minus (Const (IntVal 5)) (Const (IntVal 5))])],  
  testCase "not nesting" $
    parseString "not not 5 == 5" @?=
      Right [SExp (Not (Not (Oper Eq (Const (IntVal 5)) (Const (IntVal 5)))))],
  testCase "syntactic sugar test for order" $
    parseString "x <= 5" @?=
      Right [SExp (Not (Oper Greater (Var "x") (Const (IntVal 5))))],
  testCase "escape quote in string" $
    parseString "'ddd\'d'" @?=
      Right [SExp (Const (StringVal "ddd'd"))],
  -- testCase "escape backslash in string" $
  --   parseString "'ddd\\d'" @?=
  --     Right [SExp (Const (StringVal "ddd\d"))],
  testCase "arbitrary whitespace test" $
    parseString "5    + 5" @?=
      Right [SExp (Oper Plus (Const (IntVal 5)) (Const (IntVal 5)))],
  testCase "comments within strings" $
    case parseString "'sss#ss\n'" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "comments" $
    parseString "5#comment" @?=
      Right [SExp (Const (IntVal 5))],
  testCase "if clause and for clause test" $
    parseString "[5 for x in 3 if 4]" @?=
      Right [SExp (Compr (Const (IntVal 5)) [CCFor "x" (Const (IntVal 3)),CCIf (Const (IntVal 4))])],
  testCase "empty clause test" $
    parseString "[5 for x in 3]" @?=
      Right [SExp (Compr (Const (IntVal 5)) [CCFor "x" (Const (IntVal 3))])],
  testCase "empty clause test2" $
    parseString "[]" @?=
      Right [SExp (List [])],
  testCase "Call test" $
    parseString "func (5)" @?=
      Right [SExp (Call "func" [Const (IntVal 5)])],
  testCase "simple failure" $
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "simple failure2" $
    case parseString "sss/" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p,
  testCase "simple failure3" $
    case parseString "]]]]]" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]
