{-# LANGUAGE QuasiQuotes, DeriveDataTypeable #-} 

module TypeCheckerTests where

import Test.HUnit
import Test.HUnit.Tools
import Scanner
import Parser
import QQ
import TypeCheck
import Control.Exception 
import TypeCheckExceptions 
import Walker 

testTypeCheck :: String -> ParserState Env
testTypeCheck prog = typeCheckAST $ (parse (alexScanTokens prog))


testMismatchedAssign :: Assertion
testMismatchedAssign = assertRaises "throw exception" ( MismatchedAssignmentTypes "Mismatched types in assignment: n:=z; [checkAssignmentTypes]") (evaluate (length $ (internalParserState (testTypeCheck assignMismatch))))


testIfStmt :: Assertion
testIfStmt = assertRaises "throw exception" ( InvalidTypes "Invalid types: if(n+z){  n:=3;}else{  ;}.\n Expected: BooleanType, Actual: IntType [checkLastTypeIsA]") (evaluate (length $ (internalParserState (testTypeCheck ifStmtNotBoolean))))


assignMismatch :: String
assignMismatch = [qq|
Int n;
String z;
n := z;
|]

ifStmtNotBoolean :: String
ifStmtNotBoolean = [qq|
Int n;
Int z;

if(n+z){
  n:=3;
}
|]


