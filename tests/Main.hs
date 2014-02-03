module Main where


import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import ParserTests

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
                       ]
       , testGroup "Functions" [
                        testCase "simpleFunction" testParseFunction
                       ,testCase "functionWithParams" testParseFunctionWithParams
                       ,testCase "functionWithImplicitParams" testParseFunctionWithImplicitParams
                       ]
       , testGroup "Integration Test #1" [
                        testCase "mixed1" testParseMixedProgram 
                       ]
       ] mempty


