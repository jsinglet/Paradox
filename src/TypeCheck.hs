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

storeFunctionIdent :: Ident -> Type -> VarSpecList -> VarSpecList  -> TCParserState Env ()
storeFunctionIdent ident varType (VarSpecList formalParams) (ImplicitVarSpec (VarSpecList implicitParams)) = do 
  cl <- currLevel
  push (EnvEntry ident varType (Just (VarSpecList formalParams)) (Just (VarSpecList implicitParams)) cl) 
  -- store all the implicit and formal params 
  mapM (\(VarSpec t i)  -> do 
         push (EnvEntry i t Nothing Nothing (cl+1)) 
      ) formalParams

  mapM (\(VarSpec t i)  -> do 
         push (EnvEntry i t Nothing Nothing (cl+1)) 
      ) implicitParams

  return ()


clearLevel :: Int -> TCParserState Env ()
clearLevel l = TCParserState $ \xs -> ((), filter (\e -> level e /= l) xs)


enterBlock :: TCParserState Env ()
enterBlock = do 
  cl <- currLevel
  push (EnvEntry  "________blockEnter" VoidType Nothing Nothing (cl+1))

exitBlock :: TCParserState Env ()
exitBlock = do 
  cl <- currLevel
  clearLevel cl


 
getIdent :: Ident -> TCParserState Env (Maybe EnvEntry)
getIdent ident = TCParserState $ \xs -> (foldr (\x acc -> if (identName x == ident) then Just x else acc) Nothing xs, xs)

getLastFunctionIdent :: TCParserState Env (Maybe EnvEntry)
getLastFunctionIdent = TCParserState $ \xs -> (foldr (\x acc -> case (methodSpec x) of 
                                                                  (Just _) -> Just x 
                                                                  _ -> acc) Nothing xs, xs)


getIdentAtLevel :: Ident -> Int -> TCParserState Env (Maybe EnvEntry)
getIdentAtLevel ident l = TCParserState $ \xs -> (foldr (\x acc -> if (identName x == ident && level x == l) then Just x else acc) Nothing xs, xs)



--
-- AST Checking Functions  
--
checkFunctionSignatureMatchesReturn :: Expression -> TCParserState Env ()
checkFunctionSignatureMatchesReturn expr = 
    do 
      lhs <- pop
      mfn <- getLastFunctionIdent
      case (mfn) of
        (Just fn) -> if (identType lhs /= identType fn) then 
                         error ("Return statement does not match function signature: " ++ unparse expr 0 "" ++  " [checkFunctionSignatureMatchesReturn]") 
                     else return ()
        _ -> error ("Return statement without a matching function [checkFunctionSignatureMatchesReturn]")
      
      
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

checkFunctionAlreadyDefined :: Ident -> BlockStatement-> TCParserState Env ()
checkFunctionAlreadyDefined ident node = do 
  cl <- currLevel
  found <- getIdentAtLevel ident cl
  case found of 
    (Just _) -> error ("Variable already defined: " ++ unparse node 0 ""  ++ " [checkAlreadyDefined]")
    Nothing  -> return ()

checkIdentIsFunction :: Ident -> Expression -> TCParserState Env ()
checkIdentIsFunction ident node = do 
  found <- getIdent ident
  case found of 
    (Just i)  -> case (methodSpec i) of 
                   (Just _) -> return ()
                   Nothing  -> error ("Identifier does not appear to be a function: " ++ unparse node 0 "" ++ " [checkIdentIsFunction]")
    Nothing -> error ("Identifier not defined: " ++ unparse node 0 ""  ++ " [checkIdentIsFunction]")

--
-- TODO check that function conforms to spec.
-- 

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
  

checkFunctionParameters :: VarSpecList -> VarSpecList -> TCParserState Env ()
checkFunctionParameters (VarSpecList formalParams) (ImplicitVarSpec (VarSpecList implicitParams)) = 
    let l1 = map (\(VarSpec _ i) -> i) formalParams in let l2 = map (\(VarSpec _ i) -> i) implicitParams in case (length $ intersect l1 l2) of
                                                                                                              0 -> return ()
                                                                                                              _ -> error ("Conflicting definitions in formal and implicit params. [checkFunctionParameters]")

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

implicitTypeChecker (ParserState env) (StatementNode node@(ReturnStatement stmt)) = 
    trace ("Checking Return Type: ") $ ParserState $ let (_,s) =  runState (checkFunctionSignatureMatchesReturn stmt) env in s 


-- check that the lhs type equals the rhs type
implicitTypeChecker (ParserState env) (StatementNode node@(AssignStatement ident expression)) =     
    ParserState $ let (_,s) = runState (checkAssignmentTypes ident node) env in s 

implicitTypeChecker (ParserState env) (FactorNode (FactorIntegerLiteralExpression i )) = 
    trace ("Pushing " ++ (show i)) $ ParserState $ let (_,s) = runState (storeIdent "_infType" IntType) env in s 


implicitTypeChecker (ParserState env) (FactorNode (FactorBooleanLiteralExpression i )) = trace ("Pushing " ++ (show i)) $ ParserState $ let (_,s) = runState (storeIdent "_infType" BooleanType) env in s 


implicitTypeChecker (ParserState env) (FactorNode (FactorStringLiteralExpression i )) = trace ("Pushing " ++ (show i)) $ ParserState $ let (_,s) = runState (storeIdent "_infType" StringType) env in s

implicitTypeChecker (ParserState env) (FactorNode (IdentExpression i )) = trace ("Pushing ident " ++ (show i) ++ " with type: ") $ ParserState $ let (_,s) = runState (checkResolveIdentType i >>= storeIdent "_infType") env in s


implicitTypeChecker (ParserState env) (ExpressionNode node@(FunctionCallExpression ident actualParams)) = trace ("Pushing function ident " ++ (show ident)) $ ParserState $ let (_,s) = runState (checkIdentIsFunction ident node >> checkResolveIdentType ident >>= storeIdent "_infType") env in s


 
implicitTypeChecker (ParserState env) (ExpressionNode n) = trace ("Checking Expression: " ++ unparse n 0 "") $  ParserState $ let (_,s) = runState (checkLastTwoTypesMatch n) env in s

implicitTypeChecker (ParserState env) (EnterBlockBodyNode n) = 
    trace ("Entering block ") $ ParserState $ let (_,s) = runState (enterBlock) env in s 

implicitTypeChecker (ParserState env) (ExitBlockBodyNode n) = 
    trace ("Exiting block ") $ ParserState $ let (_,s) = runState (exitBlock) env in s 

implicitTypeChecker (ParserState env) (BlockStatementNode node@(FunctionBlockStatement varType ident formalParams implicitParams body)) = 
    trace ("\n\nEntering Function Definition: " ++ (show ident) ++ " current env: " ++ (show env) ++ "\n\n") $ ParserState $ let (_,s) =  runState (checkFunctionAlreadyDefined ident node >> checkFunctionParameters formalParams implicitParams >>  storeFunctionIdent ident varType formalParams implicitParams) env in trace ("new env: " ++ (show s) ++ "\n\n") $ s 





-- check that this var isn't already defined 
-- default case, do nothing
implicitTypeChecker before node = trace ("Skipping Node") $ before
