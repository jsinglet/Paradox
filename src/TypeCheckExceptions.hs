{-# LANGUAGE DeriveDataTypeable #-}
module TypeCheckExceptions where

import Control.Exception 
import Data.Typeable     ( Typeable )




data TypeCheckException = 
    ReturnTypeMismatch String 
    | InvalidReturnPlacement String
    | InvalidTypes String 
    | MismatchedExprType String
    | VarAlreadyDefined String
    | IdentNotFunction String
    | ImplicitParamsMismatch String
    | ImplicitParamsUndefined String
    | ActualParamsMismatch String
    | MismatchedAssignmentTypes String
    | UndeclaredIdent String
    | ConflictingDefinitions String
    deriving (Eq, Show, Ord, Typeable)

instance Exception TypeCheckException 
