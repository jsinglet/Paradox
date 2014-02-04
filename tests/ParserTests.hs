{-# LANGUAGE QuasiQuotes #-} 

module ParserTests where

import Test.HUnit
import Scanner
import Parser
import QQ
   
-- For quick parsing of strings into ASTs
testParse :: String -> Program
testParse = parse.alexScanTokens

-- Variable declarations 
testParseVarDecl :: Assertion 
testParseVarDecl = testParse "String x;" @?= Program (BlockList [BlockStatement (StatementList [LocalVarDeclStatement (VarSpec StringType "x")])])

-- All the possible variable declarations 
testParseMultipleVarDecl :: Assertion
testParseMultipleVarDecl = testParse "String x; Int y; Boolean z;" @?= Program (BlockList [BlockStatement (StatementList [LocalVarDeclStatement (VarSpec StringType "x"),LocalVarDeclStatement (VarSpec IntType "y"),LocalVarDeclStatement (VarSpec BooleanType "z")])])  

testParseAssign :: Assertion 
testParseAssign = testParse "c := 2;" @?= Program (BlockList [BlockStatement (StatementList [AssignStatement "c" (Term (Factor (FactorIntegerLiteralExpression 2)))])])  

testParseAssignExpr1 :: Assertion
testParseAssignExpr1 = testParse "d := (3 + 3);" @?= Program (BlockList [BlockStatement (StatementList [AssignStatement "d" (Term (Factor (FactorParenExpression (AddExpression (Term (Factor (FactorIntegerLiteralExpression 3))) (Factor (FactorIntegerLiteralExpression
 3))))))])]) 

testParseAssignExpr2 :: Assertion
testParseAssignExpr2 = testParse "d := (3 + a + (a * 2));" @?= Program (BlockList [BlockStatement (StatementList [AssignStatement "d" (Term (Factor (FactorParenExpression (AddExpression (AddExpression (Term (Factor (FactorIntegerLiteralExpression 3))) (Factor (IdentExpression
 "a"))) (Factor (FactorParenExpression (Term (MultiplyTerm (Factor (IdentExpression "a")) (FactorIntegerLiteralExpression 2)))))))))])])

testParseFunction :: Assertion 
testParseFunction = testParse "fn Int myFunction(){ Int x; }" @?= Program (BlockList [FunctionBlockStatement IntType "myFunction" (VarSpecList []) (ImplicitVarSpec (VarSpecList [])) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "x")]))])

testParseFunctionWithParams :: Assertion
testParseFunctionWithParams = testParse "fn Int myFunction(Int x){ Int x; }" @?= Program (BlockList [FunctionBlockStatement IntType "myFunction" (VarSpecList [VarSpec IntType "x"]) (ImplicitVarSpec (VarSpecList [])) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "x")]))])

testParseFunctionWithImplicitParams :: Assertion 
testParseFunctionWithImplicitParams = testParse "fn Int myFunction(Int x) implicitly [Int y] { Int x; }" @?= Program (BlockList [FunctionBlockStatement IntType "myFunction" (VarSpecList [VarSpec IntType "x"]) (ImplicitVarSpec (VarSpecList [VarSpec IntType "y"])) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "x")]))])

testParseFunctionWithImplicitParamsAndReturn :: Assertion 
testParseFunctionWithImplicitParamsAndReturn = testParse "fn Int myFunction(Int x) implicitly [Int y] { Int x; return 0; }" @?= Program (BlockList [FunctionBlockStatement IntType "myFunction" (VarSpecList [VarSpec IntType "x"]) (ImplicitVarSpec (VarSpecList [VarSpec IntType "y"])) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "x"),ReturnStatement (Term (Factor (FactorIntegerLiteralExpression 0)))]))])



mixedProgram :: String
mixedProgram = [qq|
Int x;
Int y;

y:=3;

fn Int myFunction() { 
     return 1; 
}
|]

testParseMixedProgram :: Assertion
testParseMixedProgram = testParse mixedProgram @?= Program (BlockList [BlockStatement (StatementList [LocalVarDeclStatement (VarSpec IntType "x"),LocalVarDeclStatement (VarSpec IntType "y"),AssignStatement "y" (Term (Factor (FactorIntegerLiteralExpression 3)))]),FunctionBlockStatement IntType "myFunction" (VarSpecList []) (ImplicitVarSpec (VarSpecList [])) (BlockBody (StatementList [ReturnStatement (Term (Factor (FactorIntegerLiteralExpression 1)))]))])


testFunctionCall1 :: Assertion
testFunctionCall1 = testParse "a := myFunction(a,b);" @?= Program (BlockList [BlockStatement (StatementList [AssignStatement "a" (FunctionCallExpression "myFunction" (ActualParametersList [Term (Factor (IdentExpression "a")),Term (Factor (IdentExpression "b"))]))])])


testFunctionCall2 :: Assertion
testFunctionCall2 = testParse "a := myFunction(a,(b+(b*1)));" @?= Program (BlockList [BlockStatement (StatementList [AssignStatement "a" (FunctionCallExpression "myFunction" (ActualParametersList [Term (Factor (IdentExpression "a")),Term (Factor (FactorParenExpression (AddExpression (Term (Factor (IdentExpression "b"))) (Factor (FactorParenExpression (Term (MultiplyTerm (Factor (IdentExpression "b")) (FactorIntegerLiteralExpression 1))))))))]))])])

testIfStatement :: Assertion
testIfStatement = testParse "if(a>b) { Int x; }" @?= Program (BlockList [BlockStatement (StatementList [IfStatement (GtExpression (Term (Factor (IdentExpression "a"))) (Factor (IdentExpression "b"))) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "x")])) (BlockBody (StatementList [Skip]))])])

testIfElseStatement :: Assertion
testIfElseStatement = testParse "if(a>b) { Int x; } else { Int y; }" @?= Program (BlockList [BlockStatement (StatementList [IfStatement (GtExpression (Term (Factor (IdentExpression "a"))) (Factor (IdentExpression "b"))) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "x")])) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "y")]))])])

testNestedIfElseStatement = testParse "if(a>b) { Int x; if(a>b) { Int y;} } else { Int y; if (a<b) { Int z;} else { Int q; }}" @?= Program (BlockList [BlockStatement (StatementList [IfStatement (GtExpression (Term (Factor (IdentExpression "a"))) (Factor (IdentExpression "b"))) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "x"),IfStatement (GtExpression (Term (Factor (IdentExpression "a"))) (Factor (IdentExpression "b"))) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "y")])) (BlockBody (StatementList [Skip]))])) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "y"),IfStatement (LtExpression (Term (Factor (IdentExpression "a"))) (Factor (IdentExpression "b"))) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "z")])) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "q")]))]))])])

testEqualityExpression :: Assertion
testEqualityExpression = testParse "if(a=b){Int y;}" @?= Program (BlockList [BlockStatement (StatementList [IfStatement (EqualsExpression (Term (Factor (IdentExpression "a"))) (Factor (IdentExpression "b"))) (BlockBody (StatementList [LocalVarDeclStatement (VarSpec IntType "y")])) (BlockBody (StatementList [Skip]))])])
