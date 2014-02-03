{-# LANGUAGE QuasiQuotes #-} 

module Main where

import System.Environment 
import QQ
import Parser
import Scanner
 

 
main :: IO ()
main = do 
  argv <- getArgs
  file <- readFile (head argv)
  let parseTree = parse (alexScanTokens file)  
  putStrLn ("Parsed AST: " ++ show(parseTree))
  print "done"



helpMessage :: String
helpMessage = [qq|Usage: paradox [-unparse | -ast] FILE
Used with no arguments, parses FILE. Reports error if there is a syntax error.

-unparse:	Display the UNPARSE of a program in FILE.
-ast:		Display the AST of program contained in FILE.
|]
