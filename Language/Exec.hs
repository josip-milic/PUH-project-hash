module Language.Exec where

import Language.Expressions
import Parsing.HashParser

import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist, canonicalizePath, getHomeDirectory, setCurrentDirectory, getCurrentDirectory)
import System.FilePath.Posix


import qualified Data.Map as M
import qualified Data.List.Split as S
import Data.List (intercalate)

-- A model of a command which is waiting for arguments and a state to run
type Command = [Expr] -> ScriptState -> IO ScriptState

-- A table of variables, in fact a map of (Name, Value) pairs.
type VarTable = M.Map String Expr

-- A command table - abstracted command execution, (contains command name,
-- command) pairs. Simplest, but hardly the best way to implement this.
type CommandTable = M.Map String Command

-- A script state containing the last output, current working directory and
-- the current table of variables.
data ScriptState = ScriptState { output   :: String
                               , wd       :: FilePath
                               , vartable :: VarTable
                               } 
                               deriving Show

-- Runs a set of commands for a given command table. If this is the first
-- command in the chain, it is given a FilePath and constructs a new, initially
-- blank, ScriptState. Otherwise, it is given the state as left by the previous
-- command's execution.
--runHashProgram :: CommandTable -> Either FilePath ScriptState -> [TLExpr] -> IO ScriptState

-- Calculates the result of a top-level command execution
--runTopLevel :: CommandTable -> ScriptState -> TLExpr -> IO ScriptState

-- The rest of the module should consist of similar functions, calling each
-- other so that each expression is parsed by a lower-level function and the
-- result can be used in a higher-level function. The Command table and state
-- are passed around as necessary to evaluate commands, assignments and
-- variable substitution. A better way to pass around variables would be to
-- use the State monad or even the StateT monad transformer to wrap IO into it.




