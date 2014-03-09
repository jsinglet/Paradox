module TypeCheck (typeCheckAST, Env) where
 
import Parser
import Data.List
import Walker
import qualified Debug.Trace as D
import UnparseShow 
import TypeCheckExceptions 
import Control.Exception 
import Data.String.Utils

-- To hold things we know about the surrounding enviorns.
-- Since we want to check implicit types we hang on to a little more information
-- about the various types of identifiers, especially the implicit method spec.
data EnvEntry = EnvEntry {
      identName :: String 
    , identType :: Type
    , methodSpec :: Maybe VarSpecList
    , implicitSpec :: Maybe VarSpecList
    , level :: Int
    } 

type Env = [EnvEntry]

instance Show EnvEntry where
    show a = let l = (identName a, identType a, level a) in (show l)

showStack :: Env -> String
showStack e = concatMap (\x -> (show x) ++ "\n-----------------------------------------------\n") e


trace :: String -> a -> a
-- quiet version 
trace _ a = a
-- loud version 
--trace s a = D.trace s a 


chomp :: String -> String 
chomp str = replace "\n" "" str

--
-- Encapsulate the state of the parser into a state Monad.
--
newtype TCParserState s a = TCParserState { runState :: s -> (a,s) }

instance Monad (TCParserState s) where  
    return x = TCParserState $ \s -> (x,s)  
    (TCParserState h) >>= f = TCParserState $ \s -> let (a, newState) = h s  
                                                        (TCParserState g) = f a  
                                                in  g newState  

newEnv :: Env
newEnv = [EnvEntry "________basePointer" VoidType Nothing Nothing 0]

--
-- TCParserState manipulation and query functions
--
initParserState :: ParserState Env
initParserState = ParserState newEnv 

pop :: TCParserState Env EnvEntry
pop = TCParserState $ \(x:xs) -> (x,xs)  

peek :: TCParserState Env EnvEntry
peek = TCParserState $ \s@(x:_) -> (x,s)  
  

push :: EnvEntry -> TCParserState Env ()  
push entry = TCParserState $ \xs -> ((),entry:xs)  

popActualParams :: TCParserState Env Env
popActualParams = TCParserState $ \xs ->  let top = takeWhile (\e -> identName e == "_infType") xs in (reverse top, drop (length top) xs)

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

clearReduceLexicalLevel :: TCParserState Env ()
clearReduceLexicalLevel = do 
  e <- pop
  if (identName e)=="________lexicalReduction" then return () else push e

reduceLexicalLevel :: VarSpecList -> VarSpecList -> TCParserState Env Int
reduceLexicalLevel (VarSpecList formalParams) (ImplicitVarSpec (VarSpecList implicitParams)) = do
  cl <- currLevel
  case ((length formalParams) + (length implicitParams) > 0) of
    True -> do
        push (EnvEntry "________lexicalReduction" VoidType Nothing Nothing (cl-1))
        return (cl-1)
    False -> 
        return (cl)

clearLevel :: Int -> TCParserState Env ()
clearLevel l = TCParserState $ \xs -> ((), filter (\e -> level e /= l) xs)

clearInf :: TCParserState Env ()
clearInf = TCParserState $ \xs -> ((), filter (\e -> identName e /= "_infType") xs)

enterBlock :: TCParserState Env ()
enterBlock = do 
  cl <- currLevel
  -- first get rid of any inf at the current level
  clearInf
  -- mark this block
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
                         throw  ( ReturnTypeMismatch $ "Return statement does not match function signature: " ++ chomp (unparse expr 0 "") ++  " [checkFunctionSignatureMatchesReturn]") 
                     else return ()
        _ -> throw (InvalidReturnPlacement "Return statement without a matching function [checkFunctionSignatureMatchesReturn]")
      
checkLastTypeIsA :: (UnparseShow a) => a -> Type -> TCParserState Env ()
checkLastTypeIsA node t = do 
  lastIdent <- peek
  case (identType lastIdent == t) of 
    True -> return ()
    False -> throw (InvalidTypes $ "Invalid types: " ++ chomp (unparse node 0 "") ++ ".\n Expected: " ++ (show t) ++ ", Actual: " ++ (show $ identType lastIdent)  ++ " [checkLastTypeIsA]")
      
checkLastTwoTypesMatch :: (UnparseShow a) => a -> TCParserState Env ()
checkLastTwoTypesMatch expr = do 
  lhs <- pop
  rhs <- pop
  push rhs
  case (identType lhs) == (identType rhs) of
    True ->  return ()
    False -> throw (MismatchedExprType $ "Type Error, Mismatched Types in Expression: " ++ chomp (unparse expr 0 "") ++ " [checkLastTwoTypesMatch]")

checkAlreadyDefined :: Ident -> Statement-> TCParserState Env ()
checkAlreadyDefined ident node = do 
  cl <- currLevel
  found <- getIdentAtLevel ident cl
  case found of 
    (Just _) -> throw (VarAlreadyDefined $ "Variable already defined: " ++ chomp (unparse node 0 "")  ++ " [checkAlreadyDefined]")
    Nothing  -> return ()

checkFunctionAlreadyDefined :: Ident -> BlockStatement-> TCParserState Env ()
checkFunctionAlreadyDefined ident node = do 
  cl <- currLevel
  found <- getIdentAtLevel ident cl
  case found of 
    (Just _) -> throw (VarAlreadyDefined $ "Variable already defined: " ++ chomp (unparse node 0 "")  ++ " [checkAlreadyDefined]")
    Nothing  -> return ()

checkIdentIsFunction :: Ident -> Expression -> TCParserState Env ()
checkIdentIsFunction ident node = do 
  found <- getIdent ident
  case found of 
    (Just i)  -> case (methodSpec i) of 
                   (Just _) -> return ()
                   Nothing  -> throw (IdentNotFunction $ "Identifier does not appear to be a function: " ++ chomp (unparse node 0 "") ++ " [checkIdentIsFunction]")
    Nothing -> throw (UndeclaredIdent $ "Identifier not defined: " ++ chomp (unparse node 0 "")  ++ " [checkIdentIsFunction]")
 
checkImplicitParams :: Ident -> Expression -> TCParserState Env ()
checkImplicitParams ident node = do 
  (Just fn) <- getIdent ident
  let (Just (VarSpecList spec)) = implicitSpec fn
  -- For each member of the implicit spec, ensure the following:
  -- 1) That it is defined in the current env
  -- 2) That the types match
  mapM (\(VarSpec t i) -> do 
          implicitParam <- getIdent i
          case (implicitParam) of 
            (Just i') -> if t==(identType i') then return () else  throw (ImplicitParamsMismatch $ "Implicit parameter types do not match in function call: " ++ chomp (unparse node 0 "") ++ " for variable: "++ (show i) ++ ".\n Expected: " ++ (show t) ++ ", Actual: " ++ (show $ identType i')  ++ " [checkImplicitParams]")
            Nothing   -> throw (ImplicitParamsUndefined $ "Implicit parameter not defined in function call: " ++ chomp (unparse node 0 "") ++ " for parameter: " ++ (show i)  ++ " [checkImplicitParams]")
       ) spec
  return ()
  


checkActualParams :: Ident -> Expression -> TCParserState Env ()
checkActualParams ident node = do 
  (Just fn) <- getIdent ident
  actualParams <- popActualParams
  let (Just (VarSpecList spec)) = methodSpec fn
  -- the intersection of their type signatures should be equal
  let lhs = map (identType) actualParams 
  let rhs = map (\(VarSpec t _) -> t) (spec)
  case (lhs == rhs) of
    True -> return ()
    False -> trace ("lhs: " ++ (show lhs) ++ " rhs: " ++ (show rhs) ) throw (ActualParamsMismatch $ "Actual Parameters do not match function formal parameters: " ++ chomp (unparse node 0 "")  ++ " [checkActualParams]") 
  
  

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
                 False -> throw (MismatchedAssignmentTypes $ "Mismatched types in assignment: " ++ chomp (unparse expr 0 "") ++ " [checkAssignmentTypes]")
    Nothing    -> throw (UndeclaredIdent $ "Undeclared identifier: " ++ (show ident) ++ " [checkAssignmentTypes]")

checkResolveIdentType :: Ident -> TCParserState Env Type
checkResolveIdentType ident = do 
  jident <- getIdent ident
  case (jident) of 
    (Just i) -> return (identType i)
    Nothing    -> throw (UndeclaredIdent $ "Undeclared identifier: " ++ (show ident) ++ " [checkResolveIdentType]")
  

checkFunctionParameters :: VarSpecList -> VarSpecList -> TCParserState Env ()
checkFunctionParameters (VarSpecList formalParams) (ImplicitVarSpec (VarSpecList implicitParams)) = 
    let l1 = map (\(VarSpec _ i) -> i) formalParams in let l2 = map (\(VarSpec _ i) -> i) implicitParams in case (length $ intersect l1 l2) of
                                                                                                              0 -> return ()
                                                                                                              _ -> throw (ConflictingDefinitions $ "Conflicting definitions in formal and implicit params. [checkFunctionParameters]") 

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
    trace ("\n\nEntering Var Definition: " ++ (show ident) ++ " Current Env:\n" ++ (showStack env) ++ "\n\n") $ ParserState $ 
          let (_,s) =  runState (checkAlreadyDefined ident node >> storeIdent ident varType) env in 
          trace ("New Env:\n" ++ (showStack s) ++ "\n\n") $ s 

implicitTypeChecker (ParserState env) (StatementNode node@(ReturnStatement stmt)) = 
    trace ("Checking Return Type. Current Env:\n" ++ (showStack env)) $ 
          ParserState $ let (_,s) =  runState (checkFunctionSignatureMatchesReturn stmt) env in s 

-- check that the lhs type equals the rhs type
implicitTypeChecker (ParserState env) (StatementNode node@(IfStatement condition trueBranch falseBranch)) =     
    trace ("Checking If Statement. Current Env:\n" ++ (showStack env))  
          ParserState $ let (_,s) = runState (checkLastTypeIsA node BooleanType) env in s 

implicitTypeChecker (ParserState env) (StatementNode node@(WhileStatement condition body)) =     
    trace ("Checking While Statement. Current Env:\n" ++ (showStack env))  
          ParserState $ let (_,s) = runState (checkLastTypeIsA node BooleanType) env in s 


-- check that the lhs type equals the rhs type
implicitTypeChecker (ParserState env) (StatementNode node@(AssignStatement ident expression)) =     
    trace ("Checking Assignment. Current Env:\n" ++ (showStack env))  ParserState $ 
          let (_,s) = runState (checkAssignmentTypes ident node) env in s 

implicitTypeChecker (ParserState env) (FactorNode (FactorIntegerLiteralExpression i )) = 
    trace ("Pushing " ++ (show i)) 
              $ ParserState $ let (_,s) = runState (storeIdent "_infType" IntType) env in  s 


implicitTypeChecker (ParserState env) (FactorNode (FactorBooleanLiteralExpression i )) = 
    trace ("Pushing " ++ (show i)) $ 
          ParserState $ let (_,s) = runState (storeIdent "_infType" BooleanType) env in s 


implicitTypeChecker (ParserState env) (FactorNode (FactorStringLiteralExpression i )) = 
    trace ("Pushing " ++ (show i)) $ ParserState $ 
          let (_,s) = runState (storeIdent "_infType" StringType) env in  s

implicitTypeChecker (ParserState env) (FactorNode (IdentExpression i )) = 
    trace ("Pushing ident " ++ (show i) ++ " with type: ") $ 
          ParserState $ let (_,s) = runState (checkResolveIdentType i >>= storeIdent "_infType") env in s


implicitTypeChecker (ParserState env) (ExpressionNode node@(FunctionCallExpression ident actualParams)) = 
    trace ("Pushing function ident: " ++ (show ident) ++  " Current Env:\n" ++ (showStack env) ++ "\n\n") $ 
          ParserState $ let (_,s) = runState (checkIdentIsFunction ident node >> checkActualParams ident node >> checkImplicitParams ident node >> checkResolveIdentType ident >>= storeIdent "_infType") env in s

implicitTypeChecker (ParserState env) (ExpressionNode n@(LtExpression expression term)) = 
    trace ("Checking LTExpression: " ++ unparse n 0 "" ++ ". Current Env:\n" ++ (showStack env) ++ "\n\n") $  
          ParserState $ let (_,s) = runState (checkLastTwoTypesMatch n >> checkLastTypeIsA n IntType >> pop >> storeIdent "_infType" BooleanType) env in s

implicitTypeChecker (ParserState env) (ExpressionNode n@(GtExpression expression term)) = 
    trace ("Checking GtExpression: " ++ unparse n 0 "" ++ ". Current Env:\n" ++ (showStack env) ++ "\n\n") $  
          ParserState $ let (_,s) = runState (checkLastTwoTypesMatch n >> checkLastTypeIsA n IntType >> pop >> storeIdent "_infType" BooleanType) env in s

implicitTypeChecker (ParserState env) (ExpressionNode n@(EqualsExpression expression term)) = 
    trace ("Checking EqualsExpression: " ++ unparse n 0 "" ++ ". Current Env:\n" ++ (showStack env) ++ "\n\n") $  
          ParserState $ let (_,s) = runState (checkLastTwoTypesMatch n >> pop >> storeIdent "_infType" BooleanType) env in s

implicitTypeChecker (ParserState env) (ExpressionNode n@(AddExpression expression term)) = 
    trace ("Checking AddExpression: " ++ unparse n 0 "" ++ ". Current Env:\n" ++ (showStack env) ++ "\n\n") $  
          ParserState $ let (_,s) = runState (checkLastTwoTypesMatch n >> checkLastTypeIsA n IntType) env in s

implicitTypeChecker (ParserState env) (ExpressionNode n@(MinusExpression expression term)) = 
    trace ("Checking MinusExpression: " ++ unparse n 0 "" ++ ". Current Env:\n" ++ (showStack env) ++ "\n\n") $  
          ParserState $ let (_,s) = runState (checkLastTwoTypesMatch n >> checkLastTypeIsA n IntType) env in s

implicitTypeChecker (ParserState env) (ExpressionNode n) = 
    trace ("Checking Expression: " ++ unparse n 0 "" ++ ". Current Env:\n" ++ (showStack env) ++ "\n\n") $  
          ParserState $ let (_,s) = runState (checkLastTwoTypesMatch n) env in s

implicitTypeChecker (ParserState env) (TermNode n@(MultiplyTerm t factor)) = 
    trace ("Checking MultiplyTerm: " ++ unparse n 0 "" ++ ". Current Env:\n" ++ (showStack env) ++ "\n\n") $  
          ParserState $ let (_,s) = runState (checkLastTwoTypesMatch n >> checkLastTypeIsA n IntType) env in s

implicitTypeChecker (ParserState env) (TermNode n@(DivideTerm t factor)) = 
    trace ("Checking DivideTerm: " ++ unparse n 0 "" ++ ". Current Env:\n" ++ (showStack env) ++ "\n\n") $  
          ParserState $ let (_,s) = runState (checkLastTwoTypesMatch n >> checkLastTypeIsA n IntType) env in s

implicitTypeChecker (ParserState env) (EnterBlockBodyNode n) = 
    trace ("Entering block ") $ ParserState $ let (_,s) = runState (enterBlock) env in s 

implicitTypeChecker (ParserState env) (ExitBlockBodyNode n) = 
    trace ("Exiting block ") $ ParserState $ let (_,s) = runState (exitBlock) env in s 

implicitTypeChecker (ParserState env) (EnterFunctionBlockStatementNode node@(FunctionBlockStatement varType ident formalParams implicitParams body)) = 
    trace ("\n\nEntering Function Definition: " ++ (show ident) ++ " Current Env:\n" ++ (showStack env) ++ "\n\n")  
          ParserState $ let (_,s) =  runState (checkFunctionAlreadyDefined ident node >> checkFunctionParameters formalParams implicitParams >>  storeFunctionIdent ident varType formalParams implicitParams >> reduceLexicalLevel formalParams implicitParams) env in s 

implicitTypeChecker (ParserState env) (ExitFunctionBlockStatementNode node@(FunctionBlockStatement varType ident formalParams implicitParams body)) = 
    trace ("\nExiting Function Definition: " ++ (show ident) ++ " Current Env:\n" ++ (showStack env) ++ "\n\n")  
          ParserState $ let (_,s) =  runState (clearReduceLexicalLevel) env in s 

-- check that this var isn't already defined 
-- default case, do nothing
implicitTypeChecker before node = trace ("Skipping Node") $ before
