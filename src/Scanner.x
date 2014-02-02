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
       "Bool"		{ \s -> TokenBoolean }
       "true"		{ \s -> TokenBooleanLiteral True }
       "false"		{ \s -> TokenBooleanLiteral False }
       "="		{ \s -> TokenEquals }
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
    | TokenBooleanLiteral Bool
    | TokenIntegerLiteral Int
    | TokenStringLiteral String
    | TokenEquals
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
    | TokenOpenBracket
    | TokenCloseBracket 
      deriving (Show, Eq)

               
-- main = do
--   argv <- getArgs
--   file <- readFile (head argv)
--   print (alexScanTokens file)

-- main = let tokens = (alexScanTokens "Int x := 3") in
--        map (\x -> putStrLn (show x)) tokens

}

