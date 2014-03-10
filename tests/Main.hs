module Main where


import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import ParserTests
import TypeCheckerTests

main :: IO ()
main = defaultMainWithOpts
       [ testGroup "Variables" [
                        testCase "varDecl" testParseVarDecl
                       ,testCase "varDeclSeq" testParseMultipleVarDecl 
                       ]
       , testGroup "Assignment" [
                        testCase "simpleAssign" testParseAssign
                       ,testCase "exprAssign"   testParseAssignExpr1
                       ,testCase "exprAssignNested" testParseAssignExpr2 
                       ,testCase "exprAssignFunction1" testFunctionCall1
                       ,testCase "exprAssignFunction2" testFunctionCall2
                       ,testCase "testEqualityExpression" testEqualityExpression
                       ]
       , testGroup "Functions" [
                        testCase "simpleFunction" testParseFunction
                       ,testCase "functionWithParams" testParseFunctionWithParams
                       ,testCase "functionWithImplicitParams" testParseFunctionWithImplicitParams
                       ,testCase "functionWithImplicitParamsAndReturn" testParseFunctionWithImplicitParamsAndReturn
                       ]
       , testGroup "Integration Test #1" [
                        testCase "mixed1" testParseMixedProgram 
                       ]

       , testGroup "Control Flow" [
                        testCase "simpleIf" testIfStatement
                       ,testCase "ifElse" testIfElseStatement
                       ,testCase "ifElseNested" testNestedIfElseStatement
                       ] 
       , testGroup "Type Checking" [
                        testCase "testMismatchedAssign" testMismatchedAssign
                       ,testCase "testIfStmt" testIfStmt
                       ,testCase "testReturnTypeMismatch" testReturnTypeMismatch
                       ,testCase "testMissingImplicitParam" testMissingImplicitParam
                       ,testCase "testMissingImplicitParam2" testMissingImplicitParam2
                       ,testCase "testImplicitParamsMismatch" testImplicitParamsMismatch
                       ]
                                 
       ] mempty


