module Walker where -- | Walker, Haskell Ranger.


import Parser
import Data.List
import UnparseShow
import Debug.Trace

-- this should be a monad eventually.
data ParserState a = ParserState {
      internalParserState :: a
    , lastError :: Maybe String
    } deriving (Show)


data ASTNode = 
    ProgramNode Program 
        | BlockListNode BlockList 
        | BlockStatementNode BlockStatement
        | StatementListNode StatementList
        | StatementNode Statement
        | BlockBodyNode BlockBody
        | ExpressionNode Expression
        | TermNode Term
        | FactorNode Factor
          deriving (Show)


-- generic tree walking function
class Walker a where
    walk :: (UnparseShow a) => a -> ParserState b -> (ParserState b -> ASTNode ->  ParserState b) -> ParserState b

instance Walker Program where
    walk node@(Program p) ps f =  walk p (f ps (ProgramNode node)) f
 

instance Walker BlockList where
    walk n@(BlockList bls) ps f = foldl (\acc a -> walk a acc f) (f ps (BlockListNode n)) bls

instance Walker BlockStatement where
    walk (BlockStatement bs) ps f = walk bs ps f
    

instance Walker StatementList where
    walk (StatementList stmts) ps f = foldl (\acc a -> walk a acc f) ps stmts

instance Walker Statement where
    walk n@(LocalVarDeclStatement _) ps f = (f ps (StatementNode n))
    walk n@(AssignStatement ident expression) ps f = (f (walk expression ps f) (StatementNode n))
    walk n@(ReturnStatement stmt) ps f = (f (walk stmt ps f) (StatementNode n))
    walk n@(Skip) ps f = (f ps (StatementNode n))
    walk n@(WhileStatement condition body) ps f = walk body (walk condition (f ps (StatementNode n)) f) f
    walk n@(FunctionCallStatement fn) ps f = (f ps (StatementNode n))

--  TODO: Need to be able to do unions.
--    walk n@(IfStatement condition trueBranch falseBranch

instance Walker BlockBody where
    walk (BlockBody body) ps f = walk body ps f

instance Walker Expression where
    -- Todo, need to do arg list

    walk n@(FunctionCallExpression ident params) ps f = ps
    -- for each of these, the union is implemented by calling the f function on a ps that has been first
    -- created for the left then the right then to the node itself. This will be the most flexible pattern. 
    walk n@(AddExpression expression term) ps f = f (walk expression (walk term ps f) f) (ExpressionNode n) 
    walk n@(MinusExpression expression term) ps f = f (walk expression (walk term ps f) f) (ExpressionNode n)
    walk n@(LtExpression expression term) ps f = f (walk expression (walk term ps f) f) (ExpressionNode n)
    walk n@(GtExpression expression term) ps f = f (walk expression (walk term ps f) f) (ExpressionNode n)
    walk n@(EqualsExpression expression term) ps f = f (walk expression (walk term ps f) f) (ExpressionNode n)
    walk n@(Term t) ps f = walk t ps f

instance Walker Term where
    walk n@(Factor factor) ps f = walk factor ps f
    walk n@(MultiplyTerm t factor) ps f = f (walk factor (walk t ps f) f) (TermNode n)
    walk n@(DivideTerm t factor) ps f =   f (walk factor (walk t ps f) f) (TermNode n)

instance Walker Factor where
    walk n@(FactorParenExpression e) ps f = f (walk e ps f) (FactorNode n)
    walk n@(FactorIntegerLiteralExpression i) ps f = f ps (FactorNode n)
    walk n@(FactorBooleanLiteralExpression b) ps f = f ps (FactorNode n)
    walk n@(FactorStringLiteralExpression s) ps f = f ps  (FactorNode n)
    walk n@(IdentExpression i) ps f = f ps (FactorNode n)
 


