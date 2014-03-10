{-# LANGUAGE QuasiQuotes #-} 

module Main where

import System.Environment  
import QQ
import Parser
import Scanner
import PrettyShow
import UnparseShow
import TypeCheck
import Walker 
import Control.Exception 
import TypeCheckExceptions 
import Data.String.Utils

-- Eval this to use flymake in Emacs so it picks up the Parser and Scanner
-- (setq ghc-ghc-options '("-i:../dist/build/paradox/paradox-tmp/"))


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
         

helpMessage :: String
helpMessage = [qq|Usage: paradox [ -unparse | -ast | -check ] FILE
-unparse:	Display the UNPARSE of a program in FILE.
-ast:		Display the AST of program contained in FILE.
-check:		Type check the program contained in FILE.
|]


dispatch :: [(String, (String -> IO ()))]  
dispatch =  [ ("-ast", doAst)  
            , ("-unparse", doUnparse)
            , ("-check", doTypeCheck)
            ]  

invalidUsage :: IO ()
invalidUsage = do
  putStrLn "Invalid Arguments"
  help


help :: IO ()
help = putStrLn helpMessage

doAst :: String -> IO ()
doAst f = do 
  file <- readFile f
  let parseTree = parse (alexScanTokens file)  
  putStrLn ("Raw AST: \n\n" ++ (show parseTree))
  putStrLn ("\nPretty AST: \n\n" ++ (prettyParse parseTree 0 ""))

doTypeCheck :: String -> IO ()
doTypeCheck f = do 
  file <- readFile f
  let parseTree = parse (alexScanTokens file)  
  putStrLn ("Raw AST: \n\n" ++ (show parseTree) ++ "\n\n")
  let parseResult = typeCheckAST parseTree
  catch (putStrLn (show (length $ (internalParserState parseResult)) ++ " idents on final stack.\nOK")) (\e -> putStrLn $ clean $ show (e :: TypeCheckException) )


doUnparse :: String -> IO ()
doUnparse f = do 
  file <- readFile f
  let parseTree = parse (alexScanTokens file)  
  putStrLn ("Raw AST: \n\n" ++ (show parseTree))
  putStrLn ("\nUnparsed Program: [Note that Sugars are removed and replaced with the AST representation] \n\n" ++ unparse parseTree 0 "")


clean :: String -> String
clean s = replace "\\\"" "\"" (replace "\\n" "\n" s)
