{
module Scanner where
import System.Environment
}





%wrapper "basic"


$digit = 0-9                     
$alpha = [a-zA-Z]                
$graphic    = $printable # $white


@string     = \" ($graphic # \")* \"


tokens :-
       $white+		;
       "//".*		;
       "Int"		{ \s -> TokenInteger }
       "String"		{ \s -> TokenString  }
       "Boolean"	{ \s -> TokenBoolean }
       "Void"		{\s -> TokenVoid }
       "true"		{ \s -> TokenBooleanLiteral True }
       "false"		{ \s -> TokenBooleanLiteral False }
       "="		{ \s -> TokenEquals }
       ">"		{ \s -> TokenGt     }
       "<"		{ \s -> TokenLt	    }
       ":="		{ \s -> TokenAssign }
       ","		{ \s -> TokenSep    }
       ";"		{ \s -> TokenSemi   }
       "+"		{ \s -> TokenAdd    }
       "-"		{ \s -> TokenMinus  }
       "*"		{ \s -> TokenMultiply }
       "/"		{ \s -> TokenDivide }
       "("		{ \s -> TokenOpenParen }
       ")"		{ \s -> TokenCloseParen }
       "]"		{ \s -> TokenCloseBracket }
       "["		{ \s -> TokenOpenBracket  }
       "fn"		{ \s -> TokenFn         }
       "return"		{ \s -> TokenReturn }
       "implicitly"	{ \s -> TokenImplicitly }
       "while"		{ \s -> TokenWhile }
       "if"		{ \s -> TokenIf    }
       "else"		{ \s -> TokenElse  }
       "{"		{ \s -> TokenOpenBrace  }
       "}" 		{ \s -> TokenCloseBrace }
       $digit+          { \s -> TokenIntegerLiteral (read s) }
       $alpha[$alpha $digit \_ \']*                { \s -> TokenIdent s }	
       @string		{ \s -> TokenStringLiteral (init (tail s)) }
       



{

data Token 
    = TokenInteger 
    | TokenString 
    | TokenBoolean 
    | TokenVoid 
    | TokenBooleanLiteral Bool
    | TokenIntegerLiteral Int
    | TokenStringLiteral String
    | TokenEquals
    | TokenLt
    | TokenGt
    | TokenAssign
    | TokenSemi
    | TokenAdd
    | TokenMinus
    | TokenMultiply
    | TokenDivide
    | TokenOpenParen
    | TokenCloseParen
    | TokenIdent String
    | TokenFn
    | TokenOpenBrace
    | TokenCloseBrace 
    | TokenSep  
    | TokenReturn
    | TokenImplicitly 
    | TokenIf
    | TokenElse
    | TokenOpenBracket
    | TokenCloseBracket 
    | TokenWhile
      deriving (Show, Eq)

               
-- main = do
--   argv <- getArgs
--   file <- readFile (head argv)
--   print (alexScanTokens file)

-- main = let tokens = (alexScanTokens "Int x := 3") in
--        map (\x -> putStrLn (show x)) tokens

}

