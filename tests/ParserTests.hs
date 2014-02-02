module ParserTests where

import Test.HUnit

import Scanner
import Parser
  

-- For quick parsing of strings into ASTs
testParse :: String -> Program
testParse = parse.alexScanTokens

-- Variable declarations 
testParseVarDecl :: Assertion
testParseVarDecl = testParse "String x;" @?= Program (Block (StatementList [LocalVarDeclStatement (VarSpec StringType "x")]))

testParseMultipleVarDecl :: Assertion
testParseMultipleVarDecl = testParse "String x; Int y; Bool

