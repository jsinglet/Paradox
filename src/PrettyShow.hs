module PrettyShow where

import Parser
import Data.List

indent :: Int -> String
indent depth = concat $ take depth (repeat "---")


class PrettyShow a where
    prettyShow :: a -> String
    prettyParse :: a -> Int -> String -> String

-- instances for pretty printing
instance PrettyShow Program where
    prettyShow _ = "Program\n"
    prettyParse b@(Program p) depth acc = prettyParse p (depth + 1) (acc ++ (prettyShow b))

instance PrettyShow BlockList where
    prettyShow _ = "BlockList [\n"
    prettyParse bl@(BlockList bls) depth acc = acc ++ (indent depth) ++(prettyShow bl) ++ (concat $ map (\b -> case b of
                                                                    bs@(BlockStatement  _ ) -> prettyParse bs (depth+1) ""
                                                                    fs@(FunctionBlockStatement _ _ _ _ _ ) -> prettyParse fs (depth+1) ""
                                                           ) bls) ++ (indent depth) ++ "]\n"
                                                                         
instance PrettyShow BlockStatement where
    prettyShow (BlockStatement _ ) = "BlockStatement: "
    prettyShow (FunctionBlockStatement _ _ _ _ _ ) = "FunctionBlockStatement:"
    prettyParse b@(BlockStatement bs) depth acc = (indent depth) ++ (prettyShow b) ++ prettyParse bs (depth+1) ""
    prettyParse fs@(FunctionBlockStatement returnType name formalParameters implicitParameters body ) depth acc  = (indent depth) ++ concat (intersperse " " [(prettyShow fs)
                                                                                                                                           , (show returnType)
                                                                                                                                           , (show name)
                                                                                                                                           , (show formalParameters)
                                                                                                                                           , (show implicitParameters), "\n"]) 
                                                                                                                   ++ prettyParse body (depth +1) " "

instance PrettyShow BlockBody where
    prettyShow (BlockBody _ ) = "BlockBody:"
    prettyParse fb@(BlockBody body) depth acc = (indent depth) ++ (prettyShow  fb) ++ prettyParse body depth acc

instance PrettyShow StatementList where
    prettyShow _ = "StatementList [\n"
    prettyParse s@(StatementList stmts) depth acc = (prettyShow s) ++ (concat $ map (\stmt -> 
                                                                                        prettyParse stmt (depth) (acc ++ (indent depth))
                                                                                   ) stmts) ++ (indent $  depth-1) ++  "]\n"
                                                                             
instance PrettyShow Statement where
    prettyShow _ = error "Not Implemented"
    prettyParse (AssignStatement ident expr) depth acc = acc ++ (indent depth) ++ "(AssignStatement " ++ (show ident) ++ " " ++ (show expr) ++ ")\n"
    prettyParse (LocalVarDeclStatement spec)  depth acc = acc ++ (indent depth) ++ "(LocalVarDeclStatement " ++ (show spec) ++ ")\n"
    prettyParse (ReturnStatement stmt) depth acc = acc ++ (indent depth) ++ "(ReturnStatement " ++ (show stmt) ++ "\n"
    prettyParse (Skip) depth acc = acc ++ "Skip" ++ "\n"
--    prettyParse (IfStatement condition trueBranch (BlockBody (StatementList [Skip]))) depth acc =  acc ++ (indent depth) ++ "(IfStatement " ++ (show condition) ++ (prettyParse trueBranch (depth+1)  "") 
    prettyParse (IfStatement condition trueBranch falseBranch) depth acc =  acc ++ (indent depth) ++ "(IfStatement " ++ (show condition) ++ "\n" ++ (prettyParse trueBranch (depth+4)  "") ++  (prettyParse falseBranch (depth+4)  "") 
