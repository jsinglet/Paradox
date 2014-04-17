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
    , udtSpec :: Maybe UDTVarSpecList
    } 

type Env = [EnvEntry]

type TypeSignature = String

instance Show EnvEntry where
    show a = let l = (identName a, identType a, level a, udtSpec a) in (show l)

showStack :: Env -> String
showStack e = concatMap (\x -> (show x) ++ "\n-----------------------------------------------\n") e


trace :: String -> a -> a
-- quiet version 
-- trace _ a = a
-- loud version 
trace s a = D.trace s a 


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
newEnv = [EnvEntry "________basePointer" VoidType Nothing Nothing 0 Nothing]

-- _infType if it's a normal ident, otherwise _infType:<functionName>
createInfTypeName :: Ident -> TCParserState Env Ident
createInfTypeName ident = do
  (Just theIdent) <- getIdent ident
  case (methodSpec theIdent) of
    (Just (VarSpecList spec)) -> trace ("Creating Function Inf Type") $ return ("_infType:" ++ ident)
    _ -> trace ("Creating Regular Inf Type") $ return ("_infType")



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

popActualParams :: Int -> TCParserState Env Env
popActualParams l = TCParserState $ \xs ->  let top = take l xs in (reverse top, drop (length top) xs)

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
  push (EnvEntry ident varType Nothing Nothing cl Nothing) 

storeUDT :: Ident -> UDTVarSpecList -> TCParserState Env ()
storeUDT ident l@(UDTVarSpecList spec) = do 
  cl <- currLevel
  push (EnvEntry ident (head spec) Nothing Nothing cl (Just l))

storeFunctionIdent :: Ident -> Type -> VarSpecList -> VarSpecList  -> TCParserState Env ()
storeFunctionIdent ident varType (VarSpecList formalParams) (ImplicitVarSpec (VarSpecList implicitParams)) = do 
  cl <- currLevel
  push (EnvEntry ident varType (Just (VarSpecList formalParams)) (Just (VarSpecList implicitParams)) cl Nothing) 
  -- store all the implicit and formal params 
  mapM (\(VarSpec t i)  -> do 
          case t of 
            (IdentType fnIdent) -> do
                   e <- (udtToEnvEntry fnIdent i (cl+1))
                   trace ("Pushing Formal Parameter (local higher order function) " ++ (show e)) $ push e
            _ -> trace ("Pushing Formal Parameter..." ++ (show t)) $ push (EnvEntry i t Nothing Nothing (cl+1) Nothing) 
      ) formalParams

  mapM (\(VarSpec t i)  -> do 
          case t of 
            (IdentType fnIdent) -> do
                   e <- (udtToEnvEntry fnIdent i (cl+1))
                   trace ("Pushing Implicit Parameter (local higher order function) " ++ (show e)) $ push e
            _ -> push (EnvEntry i t Nothing Nothing (cl+1) Nothing) 
      ) implicitParams

  return ()

udtToEnvEntry :: Ident -> String -> Int ->  TCParserState Env (EnvEntry)
udtToEnvEntry typeIdent localName cl = do 
  -- x <- getIdent typeIdent
  -- error ("found ident" ++ show x)
  -- Important: to tell these anonymous types apart from regular ones we make the udtType field an empty list.
  (Just (EnvEntry _ _ _ _ _ (Just (UDTVarSpecList formalParams)))) <- getIdent typeIdent
  return (EnvEntry localName (last formalParams) (toFormalParams (take ((length formalParams)-1) formalParams)) Nothing cl (Just $ UDTVarSpecList []))
         where
           -- this is a list of VarSpecs.
           toFormalParams params = Just (VarSpecList $ map (\e -> VarSpec e "_unbound") params)



clearReduceLexicalLevel :: TCParserState Env ()
clearReduceLexicalLevel = do 
  e <- pop
  if (identName e)=="________lexicalReduction" then return () else push e

reduceLexicalLevel :: VarSpecList -> VarSpecList -> TCParserState Env Int
reduceLexicalLevel (VarSpecList formalParams) (ImplicitVarSpec (VarSpecList implicitParams)) = do
  cl <- currLevel
  case ((length formalParams) + (length implicitParams) > 0) of
    True -> do
        push (EnvEntry "________lexicalReduction" VoidType Nothing Nothing (cl-1) Nothing)
        return (cl-1)
    False -> 
        return (cl)

clearLevel :: Int -> TCParserState Env ()
clearLevel l = TCParserState $ \xs -> ((), filter (\e -> level e /= l) xs)

clearInf :: TCParserState Env ()
clearInf = TCParserState $ \xs -> ((), filter (\e -> not $ startswith "_infType" (identName e)) xs)

enterBlock :: TCParserState Env ()
enterBlock = do 
  cl <- currLevel
  -- first get rid of any inf at the current level
  clearInf
  -- mark this block
  push (EnvEntry  "________blockEnter" VoidType Nothing Nothing (cl+1) Nothing)

exitBlock :: TCParserState Env ()
exitBlock = do 
  cl <- currLevel
  clearLevel cl


 
getIdent :: Ident -> TCParserState Env (Maybe EnvEntry)
getIdent ident = TCParserState $ \xs -> (foldr (\x acc -> if (identName x == ident) then Just x else acc) Nothing xs, xs)

getLastFunctionIdent :: TCParserState Env (Maybe EnvEntry)
getLastFunctionIdent = TCParserState $ \xs -> (foldr (\x acc -> case ((udtSpec x, methodSpec x)) of 
                                                                  (Nothing, Just _) -> Just x 
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

checkUDTAlreadyDefined :: Ident -> BlockStatement-> TCParserState Env ()
checkUDTAlreadyDefined ident node = do 
  cl <- currLevel
  found <- getIdentAtLevel ident cl
  case found of 
    (Just _) -> throw (UDTAlreadyDefined $ "UDT already defined: " ++ chomp (unparse node 0 "")  ++ " [checkUDTAlreadyDefined]")
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
  case (implicitSpec fn) of
    (Just (VarSpecList spec)) -> do 
                         -- For each member of the implicit spec, ensure the following:
                         -- 1) That it is defined in the current env
                         -- 2) That the types match


                         mapM (\ms@(VarSpec t i) -> do 
                                 -- map the specificed type to a type string
                                 ts1 <- typifyMethodSpec [ms]
                                 implicitParam <- getIdent i
                                 case (implicitParam) of 
                                   (Just i') -> do
                                     ts2 <- typifyEnvList [i']
                                     if trace ("Infered Types: ts1=" ++ (show ts1) ++ " ts2=" ++ (show ts2) ++ " Local type=" ++ (show ms)) $ ts1==ts2 then return () else  throw (ImplicitParamsMismatch $ "Implicit parameter types do not match in function call: " ++ chomp (unparse node 0 "") ++ " for variable: " ++ (show i) ++ ".\n Expected: " ++ (show ts1) ++ ", Actual: " ++ (show $ identType i')  ++ " [checkImplicitParams]")
                                   Nothing   -> throw (ImplicitParamsUndefined $ "Implicit parameter not defined in function call: " ++ chomp (unparse node 0 "") ++ " for parameter: " ++ (show i)  ++ " [checkImplicitParams]")
                              ) spec
                         return ()
    _ -> return () -- in this case it's to be ignored.

typifyEnvList :: [EnvEntry] -> TCParserState Env [TypeSignature]
typifyEnvList entries = do 
  trace (show entries) $mapM (\ident -> do 
                                -- it's a function!
                                case (startswith "_infType:" (identName ident)) of 
                                  True -> do 
                                    (Just theIdent) <- getIdent ((split ":" (identName ident)) !! 1)
                                    case ((methodSpec theIdent), (implicitSpec theIdent)) of
                                      -- yes! construct an arrow equivilant version of the function
                                      (Just (VarSpecList spec), Just (VarSpecList ispec)  ) -> return (concat $ (intersperse "->" ((map (\(VarSpec t _) -> (show t)) (spec++ispec)) ++ [show $ identType ident])))
                                      _ -> error "Found a function ident with no definition!"
                                  False -> return (show $ identType ident) 
                                
       ) entries


typifyMethodSpec :: [VarSpec] -> TCParserState Env [TypeSignature]
typifyMethodSpec entries = do 
  mapM (\x-> case x of 
              -- it's a higher order type (transform into a (a->a->a) representation.
              (VarSpec (IdentType higherOrderType) _ ) -> do 
                (Just ident) <- getIdent higherOrderType
                let spec = (udtSpec ident)
                case spec of
                  Just (UDTVarSpecList theSpecs) -> return (concat $ (intersperse "->" ((map (\s -> (show s)) theSpecs))))
                  _ -> error "Unexpected error: Found a UDT Spec with no definition!"
              -- anything else
              (VarSpec t _) -> return (show t)
      ) entries

checkActualParams :: Int -> Ident -> Expression -> TCParserState Env ()
checkActualParams l ident node = do 
  (Just fn) <- getIdent ident
  actualParams <- popActualParams l
  let (Just (VarSpecList spec)) = methodSpec fn
  -- TODO: Here we want to transform the lhs and the rhs.
  -- essentually we want to check that the type of the function we are pointing at can 
  -- match the lhs/rhs.
  lhs <- trace ("PRE LHS=:" ++ (show actualParams)) $ (typifyEnvList (actualParams)) -- = trace (show actualParams) $ map (show.identType) actualParams 
  rhs <- (typifyMethodSpec spec)-- map (\(VarSpec t _) -> t) (spec)
  -- it's possible that they passed in a function argument. 
  case (lhs == rhs) of
    True -> trace ("Check OK===" ++ "lhs: " ++ (show lhs) ++ " rhs: " ++ (show rhs)) $ return ()
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
    let l1 = map (\(VarSpec _ i) -> i) formalParams in 
    let l2 = map (\(VarSpec _ i) -> i) implicitParams in 
    case (length $ intersect l1 l2) of
      0 -> return ()
      _ -> throw (ConflictingDefinitions $ "Conflicting definitions in formal and implicit params. [checkFunctionParameters]") 

extractUDTTypes :: [VarSpec] -> [Ident]
extractUDTTypes l = map (\(VarSpec (IdentType t) _) -> t) $ filter (\e -> case e of
                                    (VarSpec (IdentType t) _) -> True 
                                    _ -> False) l

checkFunctionUDTParameters :: VarSpecList -> VarSpecList -> TCParserState Env ()
checkFunctionUDTParameters (VarSpecList formalParams) (ImplicitVarSpec (VarSpecList implicitParams)) = do
    -- combine them into one
    let combinedUDTParams = extractUDTTypes (formalParams ++ implicitParams)
    -- if there is more than one, ensure we can find each of them.
    case (length combinedUDTParams)==0 of
      True -> return ()
      _ -> do 
        mapM ( \n -> do 
                 maybeIdent <- getIdent n
                 case maybeIdent of
                   Nothing -> throw (UndeclaredUDT $ "Undeclared UDT: " ++ (show n) ++ " [checkFunctionUDTParameters]") combinedUDTParams
                   (Just theIdent) -> do 
                                   case (udtSpec theIdent) of
                                     (Just a) -> return ()
                                     Nothing  -> throw (UndeclaredUDT $ "Undeclared UDT: " ++ (show n) ++ " [checkFunctionUDTParameters]")) combinedUDTParams
        
        return ()


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


implicitTypeChecker (ParserState env) (BlockStatementNode node@(UDTStatement name signature)) =
    trace ("\n\nEntering UDT Definition: " ++ (show node) ++ " Current Env:\n" ++ (showStack env) ++ "\n\n") $ ParserState $
          let (_,s) = runState (checkUDTAlreadyDefined name node >> storeUDT name signature) env in
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
    trace ("Pushing " ++ (show i) ++ "Current Stack: " ++ (showStack env)) 
              $ ParserState $ let (_,s) = runState (storeIdent "_infType" IntType) env in  s 


implicitTypeChecker (ParserState env) (FactorNode (FactorBooleanLiteralExpression i )) = 
    trace ("Pushing " ++ (show i)) $ 
          ParserState $ let (_,s) = runState (storeIdent "_infType" BooleanType) env in s 


implicitTypeChecker (ParserState env) (FactorNode (FactorStringLiteralExpression i )) = 
    trace ("Pushing " ++ (show i)) $ ParserState $ 
          let (_,s) = runState (storeIdent "_infType" StringType) env in  s



implicitTypeChecker (ParserState env) (FactorNode (IdentExpression i )) = 
    trace ("Pushing ident " ++ (show i) ++ " with type: ") $ 
          ParserState $ let (_,s) = runState (do 
                                               t <- checkResolveIdentType i
                                               n <- createInfTypeName i
                                               (storeIdent n t)
                                             ) env in s

implicitTypeChecker (ParserState env) (StatementNode (FunctionCallStatement node@(FunctionCallExpression ident (ActualParametersList actualParams)))) = 
    trace ("Pushing function ident: " ++ (show ident) ++  " Current Env:\n" ++ (showStack env) ++ "\n\n") $ 
          ParserState $ let (_,s) = runState (checkIdentIsFunction ident node  >> checkActualParams (length actualParams) ident node >> checkImplicitParams ident node) env in s

implicitTypeChecker (ParserState env) (ExpressionNode node@(FunctionCallExpression ident (ActualParametersList actualParams))) = 
    trace ("Pushing function ident: " ++ (show ident) ++  " Current Env:\n" ++ (showStack env) ++ "\n\n") $ 
          ParserState $ let (_,s) = runState (checkIdentIsFunction ident node >> checkActualParams (length actualParams) ident node >> checkImplicitParams ident node >> checkResolveIdentType ident >>= storeIdent "_infType") env in s

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
          ParserState $ let (_,s) =  runState (checkFunctionAlreadyDefined ident node >> checkFunctionParameters formalParams implicitParams >>  checkFunctionUDTParameters formalParams implicitParams  >>  storeFunctionIdent ident varType formalParams implicitParams >> reduceLexicalLevel formalParams implicitParams) env in s 

implicitTypeChecker (ParserState env) (ExitFunctionBlockStatementNode node@(FunctionBlockStatement varType ident formalParams implicitParams body)) = 
    trace ("\nExiting Function Definition: " ++ (show ident) ++ " Current Env:\n" ++ (showStack env) ++ "\n\n")  
          ParserState $ let (_,s) =  runState (clearReduceLexicalLevel) env in s 

-- check that this var isn't already defined 
-- default case, do nothing
implicitTypeChecker before node = trace ("Skipping Node::=" ++ (show node)) $ before
