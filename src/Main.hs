module Main where

import System.Environment 

import Parser
import Scanner



 
 
main :: IO ()
main = do 
  argv <- getArgs
  file <- readFile (head argv)
  let parseTree = parse (alexScanTokens file)  
  putStrLn ("Parsed AST: " ++ show(parseTree))
  print "done"

