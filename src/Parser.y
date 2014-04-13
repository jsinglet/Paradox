
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
'>'    { TokenGt     }
'<'    { TokenLt     }
':='   { TokenAssign }
';'    { TokenSemi   }
','    { TokenSep    }
'->'   { TokenArrow  }
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
'if' { TokenIf }
'else' { TokenElse }
'return' { TokenReturn }
'data'   { TokenData   }
     

%%

Program : BlockList { Program $1 }

BlockList : BlockStatement { BlockList [$1] }
    | BlockStatement BlockList { combineBlockLists $1 $2}

BlockStatement : FunctionBlock { $1 }
    | StatementList { BlockStatement $1 }
    | UDT { $1 }
      
FunctionBlock : 'fn'  FnType ident '(' VarSpecList ')' BlockBody { FunctionBlockStatement $2 $3 $5 (ImplicitVarSpec $ VarSpecList []) $7  }
    | 'fn' FnType ident '(' VarSpecList ')' 'implicitly' '[' VarSpecList ']' BlockBody { FunctionBlockStatement $2 $3 $5 (ImplicitVarSpec $9) $11 }

StatementList :  Statement { StatementList [$1] }
    | Statement StatementList { combineStatementLists $1 $2 } 


UDT : 'data' ident '=' '(' UDTVarSpecList ')' ';' { UDTStatement $2 $5 }

UDTVarSpecList : { UDTVarSpecList [] }
    | Type { UDTVarSpecList [$1] }
    | Type '->' UDTVarSpecList { combineUDTVarSpecLists $1 $3 }


Statement : VarSpec ';' { LocalVarDeclStatement $1 }
    | ident ':=' Expression ';' { AssignStatement $1 $3 }
    | 'return' Expression ';' { ReturnStatement $2 }
    | 'while' '(' Expression ')' BlockBody { WhileStatement $3 $5 }
    | 'if'    '(' Expression ')' BlockBody { IfStatement $3 $5 (BlockBody $ StatementList [Skip]) }
    | 'if'    '(' Expression ')' BlockBody 'else' BlockBody { IfStatement $3 $5 $7 }
    | ident '(' ActualParametersList ')' ';' { FunctionCallStatement $ FunctionCallExpression $1 $3 }


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
    | ident     { IdentType $1 }

Expression : Factor { FactorExpression }
    | Expression '+' Term { AddExpression $1 $3 }
    | Expression '-' Term { MinusExpression  $1 $3 }
    | Expression '>' Term { GtExpression $1 $3 }
    | Expression '<' Term { LtExpression $1 $3 }
    | Expression '=' Term { EqualsExpression $1 $3 }
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

combineUDTVarSpecLists :: Type -> UDTVarSpecList -> UDTVarSpecList
combineUDTVarSpecLists s (UDTVarSpecList specs) = UDTVarSpecList (s:specs)



data Program 
    = Program BlockList 
      deriving (Show, Eq)

data BlockStatement
    = BlockStatement StatementList
    | FunctionBlockStatement Type Ident VarSpecList VarSpecList BlockBody 
    | UDTStatement Ident UDTVarSpecList
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

data UDTVarSpecList = 
     UDTVarSpecList [Type]
      deriving (Show, Eq)


data Statement 
    = AssignStatement Ident Expression
    | LocalVarDeclStatement VarSpec
    | ReturnStatement Expression
    | WhileStatement Expression BlockBody
    | IfStatement Expression BlockBody BlockBody
    | FunctionCallStatement Expression -- note here this should ONLY ever be FunctionCallExpression
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
    | LtExpression Expression Term
    | GtExpression Expression Term
    | EqualsExpression Expression Term
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
parseError tokens@(h:t) = error $ "Parse Error: " ++ (show tokens)

}
