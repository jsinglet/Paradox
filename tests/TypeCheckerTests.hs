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


testReturnTypeMismatch :: Assertion
testReturnTypeMismatch = assertRaises "throw exception" ( ReturnTypeMismatch "Return statement does not match function signature: True [checkFunctionSignatureMatchesReturn]") (evaluate (length $ (internalParserState (testTypeCheck returnTypeMismatch))))

testMissingImplicitParam :: Assertion
testMissingImplicitParam = assertRaises "throw exception" ( ImplicitParamsUndefined "Implicit parameter not defined in function call: foo() for parameter: \"w\" [checkImplicitParams]") (evaluate (length $ (internalParserState (testTypeCheck missingImplicitParam))))


testMissingImplicitParam2 :: Assertion
testMissingImplicitParam2 = assertRaises "throw exception" ( ImplicitParamsUndefined "Implicit parameter not defined in function call: bar() for parameter: \"x\" [checkImplicitParams]") (evaluate (length $ (internalParserState (testTypeCheck missingImplicitParam2))))


testImplicitParamsMismatch :: Assertion
testImplicitParamsMismatch = assertRaises "throw exception" ( ImplicitParamsMismatch "Implicit parameter types do not match in function call: bar() for variable: \"x\".\n Expected: [\"IntType\"], Actual: StringType [checkImplicitParams]") (evaluate (length $ (internalParserState (testTypeCheck implicitParamsTypeMismatch))))

 
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

returnTypeMismatch :: String
returnTypeMismatch = [qq|
Int n;
String z;

fn Int foo() implicitly [String w] {
   return true;
}
|]

missingImplicitParam :: String 
missingImplicitParam = [qq|
Int n;
String z;

fn Int foo() implicitly [String w] {
   return 1;
}


n := foo();
|]

missingImplicitParam2 :: String 
missingImplicitParam2 = [qq|
Int n;
String z;

fn Int bar() implicitly [Int x] {
   return x;   
}

fn Int foo()  {
   return bar();
}


foo();

|]

implicitParamsTypeMismatch :: String 
implicitParamsTypeMismatch = [qq|
Int n;
String z;

fn Int bar() implicitly [Int x] {
   return x;   
}

fn Int foo()  {
   String x;
   return bar();
}


foo();

|]
