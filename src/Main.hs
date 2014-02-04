{-# LANGUAGE QuasiQuotes #-} 

module Main where

import System.Environment 
import QQ
import Parser
import Scanner
import PrettyShow
 
-- Eval this to use flymake in Emacs so it picks up the Parser and Scanner
-- (setq ghc-ghc-options '("-i:../dist/build/paradox/paradox-tmp/"))


 
doAst :: String -> IO ()
doAst f = do 
  file <- readFile f
  let parseTree = parse (alexScanTokens file)  
  putStrLn ("Raw AST: \n\n" ++ (show parseTree))
  putStrLn ("\nPretty AST: \n\n" ++ prettyPrintAst (parseTree))

doUnparse :: String -> IO ()
doUnparse f = doAst f

main :: IO ()
main = do  
    command <- getArgs
    case command of
      (h:t) -> do
         let action = lookup h dispatch
         case action of
           Just a -> a (head t)
           Nothing -> invalidUsage
      []     -> invalidUsage
         
dispatch :: [(String, (String -> IO ()))]  
dispatch =  [ ("-ast", doAst)  
            , ("-unparse", doUnparse)  
            ]  

invalidUsage :: IO ()
invalidUsage = do
  putStrLn "Invalid Arguments"
  help


help :: IO ()
help = putStrLn helpMessage


helpMessage :: String
helpMessage = [qq|Usage: paradox [-unparse | -ast] FILE
-unparse:	Display the UNPARSE of a program in FILE.
-ast:		Display the AST of program contained in FILE.
|]


prettyPrintAst :: Program -> String
prettyPrintAst program = prettyParse program 0 ""
