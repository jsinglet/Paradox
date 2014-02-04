
{
module Parser where 
import Scanner
import System.Environment 
}

%name parse
%tokentype { Token }
%error { parseError }

%token 
"Int"    { TokenInteger }
"String" { TokenString  }
"Boolean"   { TokenBoolean }
"Void" { TokenVoid    }
boolean_literal  { TokenBooleanLiteral $$ }
integer_literal  { TokenIntegerLiteral $$ }
string_literal   { TokenStringLiteral $$  }
ident		 { TokenIdent $$      	  }
'='    { TokenEquals }
':='   { TokenAssign }
';'    { TokenSemi   }
','    { TokenSep    }
'+'    { TokenAdd   }
'-'    { TokenMinus  }
'*'    { TokenMultiply  }
'/'    { TokenDivide    }
'('    { TokenOpenParen  }
'{'    { TokenOpenBrace  }
'}'    { TokenCloseBrace }
')'    { TokenCloseParen }
']'    { TokenCloseBracket }
'['    { TokenOpenBracket }
'fn'   { TokenFn         }
'while' { TokenWhile }
'implicitly' { TokenImplicitly }
'return' { TokenReturn }
     

%%

Program : BlockList { Program $1 }

BlockList : BlockStatement { BlockList [$1] }
    | BlockStatement BlockList { combineBlockLists $1 $2}

BlockStatement : FunctionBlock { $1 }
    | StatementList { BlockStatement $1 }
      
FunctionBlock : 'fn'  FnType ident '(' VarSpecList ')' BlockBody { FunctionBlockStatement $2 $3 $5 (ImplicitVarSpec $ VarSpecList []) $7  }
    | 'fn' FnType ident '(' VarSpecList ')' 'implicitly' '[' VarSpecList ']' BlockBody { FunctionBlockStatement $2 $3 $5 (ImplicitVarSpec $9) $11 }


StatementList :  Statement { StatementList [$1] }
    | Statement StatementList { combineStatementLists $1 $2 } 


Statement : VarSpec ';' { LocalVarDeclStatement $1 }
    | ident ':=' Expression ';' { AssignStatement $1 $3 }
    | 'return' Expression ';' { ReturnStatement $2 }
    | 'while' '(' Expression ')' BlockBody { WhileStatement $3 $5 }
    | 'while' '(' Expression ')' BlockBody { WhileStatement $3 $5 }

VarSpecList : { VarSpecList [] }
    | VarSpec { VarSpecList [$1] }
    | VarSpec ',' VarSpecList { combineVarSpecLists $1 $3 }

VarSpec : Type ident { VarSpec $1 $2 }

ActualParametersList : { ActualParametersList [] }
    | Expression { ActualParametersList [$1]}
    | Expression ',' ActualParametersList { combineActualParametersLists $1 $3 }




BlockBody : '{' '}' { BlockBody (StatementList [Skip]) }
    | '{' StatementList '}' { BlockBody $2 }
   
	
FnType : Type { $1 }
    | "Void" {VoidType}

Type : "Int" {IntType}
    | "String" {StringType}
    | "Boolean" {BooleanType}


Expression : Factor { FactorExpression }
    | Expression '+' Term { AddExpression $1 $3 }
    | Expression '-' Term { MinusExpression  $1 $3 }
    | Term                { Term $1 }
    | ident '(' ActualParametersList ')' { FunctionCallExpression $1 $3 }



Factor : '(' Expression ')'	{ FactorParenExpression $2 }
    | boolean_literal        { FactorBooleanLiteralExpression $1 }
    | integer_literal        { FactorIntegerLiteralExpression  $1}
    | string_literal		{ FactorStringLiteralExpression $1  }
    | ident			{ IdentExpression $1 }

Term : Term '*' Factor { MultiplyTerm $1 $3 }
    | Term '/' Factor { DivideTerm $1 $3  }
    | Factor          { Factor $1 }


{

-- | Data types for productions
combineActualParametersLists :: Expression -> ActualParametersList -> ActualParametersList
combineActualParametersLists a (ActualParametersList params) = ActualParametersList (a:params)

combineBlockLists :: BlockStatement -> BlockList -> BlockList
combineBlockLists b (BlockList blocks) = BlockList (b:blocks)

combineStatementLists :: Statement -> StatementList -> StatementList
combineStatementLists s (StatementList stats) = StatementList (s:stats)

combineVarSpecLists :: VarSpec -> VarSpecList -> VarSpecList
combineVarSpecLists s (VarSpecList specs) = VarSpecList (s:specs)

data Program 
    = Program BlockList 
      deriving (Show, Eq)

data BlockStatement
    = BlockStatement StatementList
    | FunctionBlockStatement Type Ident VarSpecList VarSpecList BlockBody 
      deriving (Show, Eq)

data BlockList
     = BlockList [BlockStatement]
      deriving (Show, Eq)

data VarSpec = 
     VarSpec Type Ident
      deriving (Show, Eq)


data ActualParametersList = 
    ActualParametersList [Expression]
     deriving (Show, Eq)

data VarSpecList = 
     VarSpecList [VarSpec]
     | ImplicitVarSpec VarSpecList
     | EmptySpec 
      deriving (Show, Eq)

data Statement 
    = AssignStatement Ident Expression
    | LocalVarDeclStatement VarSpec
    | ReturnStatement Expression
    | WhileStatement Expression BlockBody
    | Skip
      deriving (Show, Eq)

data BlockBody
     = BlockBody StatementList
      deriving (Show, Eq)

data StatementList 
     = StatementList [Statement]
     deriving (Show, Eq)

data Expression 
    = FactorExpression 
    | AddExpression Expression Term
    | MinusExpression Expression Term
    | Term Term
    | FunctionCallExpression Ident ActualParametersList
      deriving (Show, Eq)
               
data Factor 
    = FactorParenExpression Expression 
    | FactorIntegerLiteralExpression Int
    | FactorBooleanLiteralExpression Bool
    | FactorStringLiteralExpression String
    | IdentExpression Ident
      deriving (Show, Eq)
               
data Term 
    = MultiplyTerm Term Factor
    | DivideTerm Term Factor
    | Factor Factor
      deriving (Show, Eq)
               

data Type 
    = IntType 
    | StringType
    | BooleanType 
    | VoidType
    | IdentType Ident
      deriving (Show, Eq)  
               
type Ident 
    = String
type Integer_Literal 
    = Int
type String_Literal 
    = String
type Boolean_Literal 
    = Bool

parseError :: [Token] -> a
parseError _ = error "Parse Error!"

}
