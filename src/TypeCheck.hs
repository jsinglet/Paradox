module TypeCheck where

import Parser
import Data.List

 
-- To hold things we know about the surrounding enviorns.
-- Since we want to check implicit types we hang on to a little more information
-- about the various types of identifiers, especially the implicit method spec.
data EnvEntry = EnvEntry {
      identName :: String
    , identType :: Type
    , methodSpec :: Maybe VarSpecList
    , implicitSpec :: Maybe VarSpecList
    }

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
