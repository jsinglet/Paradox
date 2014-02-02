module Main where


import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import ParserTests

main :: IO ()
main = defaultMainWithOpts
       [ testCase "varDecl" testParseVarDecl
       ] mempty

