module UnparseShow where

import Parser
import Scanner
import Data.List

 
indent :: Int -> String
indent depth = concat $ take depth (repeat "  ")

tokenMap :: [(Token,String)]
tokenMap =  [ (TokenAssign, ":=")
            , (TokenSemi, ";")
            , (TokenReturn, "return")
            , (TokenOpenBrace, "{")
            , (TokenCloseBrace, "}")
            , (TokenOpenParen, "(")
            , (TokenCloseParen, ")")
            , (TokenWhile, "while")
            , (TokenIf, "if")
            , (TokenElse, "else")
            , (TokenOpenBracket, "[")
            , (TokenCloseBracket, "]")
            , (TokenFn, "fn")
            , (TokenImplicitly, "implicitly")
            , (TokenSep, ",")
            , (TokenAdd, "+")
            , (TokenMinus, "-")
            , (TokenMultiply, "*")
            , (TokenDivide, "/")
            , (TokenGt, ">")
            , (TokenLt, "<")
            , (TokenEquals, "=")]  

mapToken :: Token -> String
mapToken t = do
         let action = lookup t tokenMap
         case action of
           Just a -> a
           Nothing -> error "Invalid Token"


class UnToken a where
    untoken :: a -> String

instance UnToken Type where
    untoken (IntType) = "Int"
    untoken (StringType) = "String"
    untoken (BooleanType) = "Boolean"
    untoken (VoidType)    = "Void"

class UnparseShow a where
    unparse :: a -> Int -> String -> String


instance UnparseShow Program where
    unparse (Program p) depth acc = unparse p depth acc

instance UnparseShow BlockList where
    unparse (BlockList bls) depth acc = acc 
                                           ++ (concat $ map (\b -> 
                                                                 case b of
                                                                   bs@(BlockStatement _) -> unparse bs depth  ""
                                                                   fs@(FunctionBlockStatement _ _ _ _ _ ) -> unparse fs depth ""
                                                            ) bls )
                                                                                        

instance UnparseShow BlockStatement where
    unparse (BlockStatement bs) depth acc = acc ++ (unparse bs depth "")
    unparse (FunctionBlockStatement returnType name formalParameters implicitParameters body) depth acc = acc ++ (indent depth) ++ (mapToken TokenFn) ++ " " ++  (untoken returnType) ++ " " ++ name ++ (mapToken TokenOpenParen) ++ (unparse formalParameters depth acc)  ++ (mapToken TokenCloseParen) ++ " " ++ (unparse implicitParameters depth acc) ++ (unparse body depth acc) 


instance UnparseShow ActualParametersList where
    unparse (ActualParametersList params) depth acc = concat $ intersperse ", " (map (\param -> unparse param depth "") params)

instance UnparseShow VarSpecList where 
    unparse (VarSpecList specs) depth acc = concat $ intersperse ", " (map (\spec -> unparse spec depth "") specs)
    unparse (ImplicitVarSpec vsl) depth acc = (mapToken TokenImplicitly) ++ " " ++ (mapToken TokenOpenBracket) ++ (unparse vsl depth acc) ++ (mapToken TokenCloseBracket)

instance UnparseShow VarSpec where
    unparse (VarSpec varType ident) depth acc = (untoken varType) ++ " " ++ ident

instance UnparseShow BlockBody where
    unparse (BlockBody body) depth acc = (mapToken TokenOpenBrace) ++ "\n" ++ (unparse body (depth + 1) acc) ++ (indent depth) ++  (mapToken TokenCloseBrace) ++ "\n" 

instance UnparseShow StatementList where
    unparse (StatementList stmts) depth acc = concat $ map (\stmt ->
                                                                  unparse stmt depth acc
                                                             ) stmts

instance UnparseShow Statement where
    unparse (LocalVarDeclStatement varspec) depth acc = acc ++ (indent depth) ++ (unparse varspec depth acc) ++ (mapToken TokenSemi) ++ "\n"
    unparse (AssignStatement ident expression) depth acc = acc ++ (indent depth) ++ ident ++ (mapToken TokenAssign) ++ (unparse expression depth "") ++ (mapToken TokenSemi) ++ "\n"
    unparse (ReturnStatement stmt) depth acc = acc ++ (indent depth) ++ (mapToken TokenReturn) ++ " " ++ (unparse stmt depth "") ++ (mapToken TokenSemi) ++ "\n"
    unparse (Skip) depth acc = acc ++ (indent depth) ++ (mapToken TokenSemi) ++ "\n"
    unparse (WhileStatement condition body) depth acc = acc ++ (indent depth) ++ (mapToken TokenWhile) ++ (mapToken TokenOpenParen) ++ (unparse condition depth acc) ++ (mapToken TokenCloseParen) ++ (unparse body depth acc) ++ "\n"
    unparse (IfStatement condition trueBranch falseBranch) depth acc = acc ++ (indent depth) ++ (mapToken TokenIf) ++ (mapToken TokenOpenParen) ++ (unparse condition depth acc) ++ (mapToken TokenCloseParen) ++ (unparse trueBranch depth acc) ++ (indent depth) ++ (mapToken TokenElse) ++ (unparse falseBranch depth acc)
    unparse (FunctionCallStatement fn) depth acc = acc ++ (indent depth) ++ (unparse fn depth acc) ++ (mapToken TokenSemi) ++ "\n"

-- unroll all the various kinds of expressions
instance UnparseShow Expression where
    unparse (FunctionCallExpression ident params) depth acc = (indent depth) ++ ident ++ (mapToken TokenOpenParen) ++ (unparse params depth acc) ++ (mapToken TokenCloseParen)
    unparse (AddExpression expression term) depth acc = (unparse expression depth acc) ++ (mapToken TokenAdd) ++ (unparse term depth acc)
    unparse (MinusExpression expression term) depth acc = (unparse expression depth acc) ++ (mapToken TokenMinus) ++ (unparse term depth acc)
    unparse (LtExpression expression term) depth acc = (unparse expression depth acc) ++ (mapToken TokenLt) ++ (unparse term depth acc)
    unparse (GtExpression expression term) depth acc = (unparse expression depth acc) ++ (mapToken TokenGt) ++ (unparse term depth acc)
    unparse (EqualsExpression expression term) depth acc = (unparse expression depth acc) ++ (mapToken TokenEquals) ++ (unparse term depth acc)


    unparse (Term f) depth acc = (unparse f depth acc)
    unparse expression depth acc = (show expression)


instance UnparseShow Term where
    unparse (Factor f) depth acc = (unparse f depth acc)
    unparse (MultiplyTerm t f) depth acc  = (unparse t depth acc) ++ (mapToken TokenMultiply) ++ (unparse f depth acc)
    unparse (DivideTerm t f) depth acc  = (unparse t depth acc) ++ (mapToken TokenDivide) ++ (unparse f depth acc)


instance UnparseShow Factor where
    unparse (FactorParenExpression e) depth acc = (mapToken TokenOpenParen) ++ (unparse e depth acc) ++ (mapToken TokenCloseParen)
    unparse (FactorIntegerLiteralExpression i) depth acc = show i
    unparse (FactorBooleanLiteralExpression b) depth acc = show b
    unparse (FactorStringLiteralExpression s) depth acc = show s
    unparse (IdentExpression i) depth acc = i


