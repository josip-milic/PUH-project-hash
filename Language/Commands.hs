module Language.Commands where

import Language.Exec
import Language.Expressions
import Language.Evals

import Data.List (intercalate)

import qualified Data.Map as M
import qualified Data.List.Split as S
import Data.List (intercalate)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist, canonicalizePath, getHomeDirectory, setCurrentDirectory, getCurrentDirectory)
import System.FilePath.Posix



-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = M.fromList 
           [ ("echo" , echoCommand)
           , ("pwd"  , pwdCommand )
           , ("ls"   , lsCommand )
           , ("cd"   , cdCommand )
           , ("cat"   , catCommand )
           ]

echoCommand :: [Expr] -> ScriptState -> IO ScriptState
echoCommand args scr@(ScriptState out wd vart) =  do
  case (null args) of
    True  -> error "Command 'echo' requires at least one string, value or $variable."
    False -> do  case (head args) of
                   Str str -> return $ ScriptState (replaceVar str) wd vart
                   _       -> return $ ScriptState (show (unwrap (evalCalc (head args) vart))) wd vart
  where unwrap (Num n) = n
        replaceVar str = intercalate " " $ fmap f $ S.splitOn " " str
        f s = if ((not . null) s) then 
                case (head s == '$') of
                  True -> case (M.lookup (tail s) vart) of
                            Nothing -> s
                            Just v  -> show $ unwrap $ evalCalc v vart
                  False -> s
              else s


--https://hackage.haskell.org/package/filepath-1.3.0.2/docs/System-FilePath-Posix.html

lsCommand :: [Expr] -> ScriptState -> IO ScriptState
lsCommand [] scr@(ScriptState out wd vart) = do
  dirls <- getDirectoryContents wd
  name <- canonicalizePath wd
  let namedec = (replicate (length name) '=')++"\n"++
                 name++"\n"++
                (replicate (length name) '=')++"\n\n"
  let ls = namedec ++ intercalate "\n" dirls
  return $ ScriptState ls wd vart
lsCommand ((Str arg):as) scr@(ScriptState out wd vart) = do
  let rel = isRelative arg
  let fp  = if (rel) then (joinPath [wd,arg]) else do arg
  
  abs <- doesDirectoryExist fp
  case (abs) of
    True -> do 
              name <- canonicalizePath fp
              let namedec = (replicate (length name) '=')++"\n"++
                            name++"\n"++
                            (replicate (length name) '=')++"\n\n"
              dirls <- getDirectoryContents fp
              let ls = namedec ++ intercalate "\n" dirls
              case (length as > 0) of
                True  -> lsCommand as $ ScriptState (out++"\n"++ls) wd vart
                False -> return $ ScriptState (out++"\n"++ls) wd vart
    False -> do case (length as > 0) of
                  True  -> lsCommand as $ ScriptState (out++"\n"++"Directory with name '"++fp++"' not found") wd vart
                  False -> return $ ScriptState (out++"\n"++"Directory with name '"++fp++"' not found") wd vart
lsCommand _ scr = return scr

--https://hackage.haskell.org/package/filepath-1.3.0.2/docs/System-FilePath-Posix.html


cdCommand :: [Expr] -> ScriptState -> IO ScriptState
cdCommand [] scr@(ScriptState out wd vart) = do
  cd <- getHomeDirectory 
  return $ ScriptState ("Directory changed to '"++cd++"'") cd vart

cdCommand ((Str arg):_) scr@(ScriptState out wd vart) = do
  let rel = isRelative arg
  let fp  = if (rel) then (joinPath [wd,arg]) else do arg
  abs <- doesDirectoryExist fp
  case abs of
    True -> return $ ScriptState ("Directory changed to '"++fp++"'") fp vart
    False -> return $ ScriptState ("Directory '"++arg++"' not found.") wd vart

pwdCommand :: [Expr] -> ScriptState -> IO ScriptState
pwdCommand args scr@(ScriptState out wd vart) =  do
  return $ ScriptState wd wd vart

catCommand :: [Expr] -> ScriptState -> IO ScriptState
catCommand [] scr = do
  return $ scr
catCommand ((Str arg):as) scr@(ScriptState out wd vart) = do
  abs <- doesFileExist arg
  case (abs) of
    True -> do 
              name <- canonicalizePath arg
              let namedec = (replicate (length name) '=')++"\n"++
                            name++"\n"++
                            (replicate (length name) '=')++"\n\n"
              
              file <- readFile arg
              let cat = namedec ++ file
              case (length as > 0) of
                True  -> catCommand as $ ScriptState (out++"\n"++cat) wd vart
                False -> return $ ScriptState (out++"\n"++cat) wd vart
    False -> do case (length as > 0) of
                  True  -> lsCommand as $ ScriptState (out++"\n"++"File with name '"++arg++"' not found") wd vart
                  False -> return $ ScriptState (out++"\n"++"File with name '"++arg++"' not found") wd vart
catCommand _ scr = return scr
        
