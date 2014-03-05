module TypeCheck where

import Parser
import Data.List
import Walker
import Debug.Trace
import UnparseShow

-- To hold things we know about the surrounding enviorns.
-- Since we want to check implicit types we hang on to a little more information
-- about the various types of identifiers, especially the implicit method spec.
data EnvEntry = EnvEntry {
      identName :: String
    , identType :: Type
    , methodSpec :: Maybe VarSpecList
    , implicitSpec :: Maybe VarSpecList
    , level :: Int
    } deriving (Show)

-- A stack of environments. To make it easier to find identifiers, we index by
-- the Ident field. 
type Env = [(Ident,EnvEntry)]

-- get a new environment
newEnv :: Env
newEnv = []

-- push an ident into the environment
push :: Env -> EnvEntry -> Env
push env e =  (identName e, e) : env

-- get the last ident off of the environment
pop :: Env -> (EnvEntry, Env)
pop env = (snd $ head env, tail env)

-- attempt to find an ident in a given environment
getIdent :: Ident -> Env -> Maybe EnvEntry
getIdent ident env = lookup ident env

getIdentAtLevel :: Ident -> Int -> Env -> Maybe EnvEntry
getIdentAtLevel ident l env = foldl (\acc (i,entry) -> if i==ident && (level entry)==l then (Just entry) else acc) Nothing env


checkLastTwoTypesMatch :: Env -> Expression -> Env
checkLastTwoTypesMatch env expr = let (lhs, e1) = pop env in let (rhs, e2) = pop e1 in case (identType lhs)==(identType rhs) of 
                                                                                   True -> e1
                                                                                   False -> error ("Type Error, Mismatched Types in Expression: " ++ unparse expr 0 "")

currLevel :: Env -> Int
currLevel [] = 0
currLevel (h:_) = level (snd h) 

--  
typeCheckAST :: Program -> ParserState Env
typeCheckAST p = walk p (ParserState newEnv Nothing) implicitTypeChecker

implicitTypeChecker :: ParserState Env -> ASTNode -> ParserState Env

-- local var decl statements enter new definitions. 
implicitTypeChecker (ParserState env lm) (StatementNode node@(LocalVarDeclStatement (VarSpec varType ident))) 
    | (alreadyDefined==False) = (ParserState (push env (EnvEntry ident varType Nothing Nothing cl)) lm) 
    | otherwise = error ("Variable already defined: " ++ unparse node 0 "" )
    where alreadyDefined = case (getIdentAtLevel ident cl env) of 
                             Nothing -> False 
                             _ -> True  
          cl = currLevel env

-- check that the lhs type equals the rhs type
implicitTypeChecker (ParserState env lm) (StatementNode node@(AssignStatement ident expression)) = let lhs = resolveLHS in let (rhs,e2) = pop env in 
                                                                                                                                   case (identType lhs)==(identType rhs) of
                                                                                                                                     True -> ParserState e2 lm
                                                                                                                                     False -> error ("Mismatched types in assignment: " ++ unparse node 0 "")
                                                                                                           where
                                                                                                             resolveLHS = let i = getIdent ident env in 
                                                                                                                          case (i) of
                                                                                                                            Just n -> n
                                                                                                                            _      -> error ("Undeclared identifier: " ++ (show i))


implicitTypeChecker (ParserState env lm) (FactorNode (FactorIntegerLiteralExpression i )) = trace ("Pushing " ++ (show i)) $ ParserState (push env (EnvEntry "_infType" IntType Nothing Nothing (currLevel env))) lm

implicitTypeChecker (ParserState env lm) (FactorNode (FactorBooleanLiteralExpression i )) = trace ("Pushing " ++ (show i)) $ ParserState (push env (EnvEntry "_infType" BooleanType Nothing Nothing (currLevel env))) lm


implicitTypeChecker (ParserState env lm) (FactorNode (FactorStringLiteralExpression i )) = trace ("Pushing " ++ (show i)) $ ParserState (push env (EnvEntry "_infType" StringType Nothing Nothing (currLevel env))) lm


implicitTypeChecker (ParserState env lm) (FactorNode (IdentExpression i )) = trace ("Pushing ident " ++ (show i) ++ " with type: " ++ (show iType)) $ ParserState (push env (EnvEntry "_infType" iType Nothing Nothing (currLevel env))) lm
                                                                             where
                                                                               iType = let ident = getIdent i env in 
                                                                                       case (ident) of
                                                                                         Nothing -> error ("Undeclared identifier: " ++ (show i))
                                                                                         Just theIdent       -> identType theIdent

 
implicitTypeChecker (ParserState env lm) (ExpressionNode n) = trace ("Checking Expression: " ++ unparse n 0 "") ParserState (checkLastTwoTypesMatch env n) lm

-- check that this var isn't already defined 
-- default case, do nothing
implicitTypeChecker before node = trace ("Skipping Node" ++ show node) $ before
