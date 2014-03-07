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

--
-- Encapsulate the state of the parser into a state Monad.
--
newtype TCParserState s a = TCParserState { runState :: s -> (a,s) }

instance Monad (TCParserState s) where  
    return x = TCParserState $ \s -> (x,s)  
    (TCParserState h) >>= f = TCParserState $ \s -> let (a, newState) = h s  
                                                        (TCParserState g) = f a  
                                                in  g newState  

type Env = [EnvEntry]

newEnv :: Env
newEnv = [EnvEntry "________basePointer" VoidType Nothing Nothing 0]

--
-- TCParserState manipulation and query functions
--
initParserState :: ParserState Env
initParserState = ParserState newEnv 

pop :: TCParserState Env EnvEntry
pop = TCParserState $ \(x:xs) -> (x,xs)  
  
push :: EnvEntry -> TCParserState Env ()  
push entry = TCParserState $ \xs -> ((),entry:xs)  


get :: TCParserState Env ()  
get = TCParserState $ \xs -> ((),xs)  

currLevel :: TCParserState Env Int
currLevel =  do 
  top <- pop
  push top
  return (level top)


storeIdent :: Ident -> Type -> TCParserState Env ()
storeIdent ident varType = do 
  cl <- currLevel
  push (EnvEntry ident varType Nothing Nothing cl) 

 
getIdent :: Ident -> TCParserState Env (Maybe EnvEntry)
getIdent ident = TCParserState $ \xs -> (foldr (\x acc -> if (identName x == ident) then Just x else acc) Nothing xs, xs)

getIdentAtLevel :: Ident -> Int -> TCParserState Env (Maybe EnvEntry)
getIdentAtLevel ident l = TCParserState $ \xs -> (foldr (\x acc -> if (identName x == ident && level x == l) then Just x else acc) Nothing xs, xs)



--
-- AST Checking Functions  
--
checkLastTwoTypesMatch :: Expression -> TCParserState Env ()
checkLastTwoTypesMatch expr = do 
  lhs <- pop
  rhs <- pop
  push rhs
  case (identType lhs) == (identType rhs) of
    True -> return ()
    False -> error ("Type Error, Mismatched Types in Expression: " ++ unparse expr 0 "" ++ " [checkLastTwoTypesMatch]")

checkAlreadyDefined :: Ident -> Statement-> TCParserState Env ()
checkAlreadyDefined ident node = do 
  cl <- currLevel
  found <- getIdentAtLevel ident cl
  case found of 
    (Just _) -> error ("Variable already defined: " ++ unparse node 0 ""  ++ " [checkAlreadyDefined]")
    Nothing  -> return ()

checkAssignmentTypes :: Ident -> Statement-> TCParserState Env ()
checkAssignmentTypes ident expr = do 
  jlhs <- getIdent ident
  case (jlhs) of 
    (Just lhs) -> do 
               rhs <- pop
               case (identType rhs)==(identType lhs) of
                 True -> return ()
                 False -> error ("Mismatched types in assignment: " ++ unparse expr 0 "" ++ "[checkAssignmentTypes]")
    Nothing    -> error ("Undeclared identifier: " ++ (show ident) ++ " [checkAssignmentTypes]")

checkResolveIdentType :: Ident -> TCParserState Env Type
checkResolveIdentType ident = do 
  jident <- getIdent ident
  case (jident) of 
    (Just i) -> return (identType i)
    Nothing    -> error ("Undeclared identifier: " ++ (show ident) ++ " [checkResolveIdentType]")
  

--
-- Main hook
--
typeCheckAST :: Program -> ParserState Env
typeCheckAST p = walk p (ParserState newEnv) implicitTypeChecker



--
-- Entrypoints for the walker to call into as we reach various nodes.
--

implicitTypeChecker :: ParserState Env -> ASTNode -> ParserState Env

-- local var decl statements enter new definitions. 
implicitTypeChecker (ParserState env) (StatementNode node@(LocalVarDeclStatement (VarSpec varType ident))) = 
    trace ("\n\nEntering Var Definition: " ++ (show ident) ++ " current env: " ++ (show env) ++ "\n\n") $ ParserState $ let (_,s) =  runState (checkAlreadyDefined ident node >> storeIdent ident varType) env in trace ("new env: " ++ (show s) ++ "\n\n") $ s 

-- check that the lhs type equals the rhs type
implicitTypeChecker (ParserState env) (StatementNode node@(AssignStatement ident expression)) =     
    ParserState $ let (_,s) = runState (checkAssignmentTypes ident node) env in s 

implicitTypeChecker (ParserState env) (FactorNode (FactorIntegerLiteralExpression i )) = 
    trace ("Pushing " ++ (show i)) $ ParserState $ let (_,s) = runState (storeIdent "_infType" IntType) env in s 


implicitTypeChecker (ParserState env) (FactorNode (FactorBooleanLiteralExpression i )) = trace ("Pushing " ++ (show i)) $ ParserState $ let (_,s) = runState (storeIdent "_infType" BooleanType) env in s 


implicitTypeChecker (ParserState env) (FactorNode (FactorStringLiteralExpression i )) = trace ("Pushing " ++ (show i)) $ ParserState $ let (_,s) = runState (storeIdent "_infType" StringType) env in s

implicitTypeChecker (ParserState env) (FactorNode (IdentExpression i )) = trace ("Pushing ident " ++ (show i) ++ " with type: ") $ ParserState $ let (_,s) = runState (checkResolveIdentType i >>= storeIdent "_infType") env in s

 
implicitTypeChecker (ParserState env) (ExpressionNode n) = trace ("Checking Expression: " ++ unparse n 0 "") $  ParserState $ let (_,s) = runState (checkLastTwoTypesMatch n) env in s

-- check that this var isn't already defined 
-- default case, do nothing
implicitTypeChecker before node = trace ("Skipping Node" ++ show node) $ before
