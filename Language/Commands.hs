module Language.Commands where

import Language.Exec
import Language.Expressions
import Language.Evals

import Data.List (intercalate)

import qualified Data.Map as M
import qualified Data.List.Split as S
import Data.List (intercalate)
import System.Directory
import System.FilePath.Posix



-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy
commands :: M.Map String Command
commands = M.fromList 
           [ ("echo" , echoCommand )
           , ("pwd"  , pwdCommand  )
           , ("ls"   , lsCommand   )
           , ("cd"   , cdCommand   )
           , ("cat"  , catCommand  )
           , ("mv"   , mvCommand   )
           ]

-- COMMAND ECHO
echoCommand :: [Expr] -> ScriptState -> IO ScriptState
echoCommand [] (ScriptState out wd vart) = do
  return $ ScriptState "No argument (value or string) given." wd vart
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

-- COMMAND LS
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
  fp  <- canonicalizePath arg
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
    False -> do let sserr = ScriptState (out++
                                          "\n\n"++"Directory with name '"++
                                          fp++"' not found") wd vart
                case (length as > 0) of
                  True  -> lsCommand as sserr
                  False -> return sserr
lsCommand _ scr = return scr

--https://hackage.haskell.org/package/filepath-1.3.0.2/docs/System-FilePath-Posix.html



-- COMMAND CD 
cdCommand :: [Expr] -> ScriptState -> IO ScriptState
cdCommand [] scr@(ScriptState out wd vart) = do
  cd <- getHomeDirectory 
  return $ ScriptState ("Directory changed to '"++cd++"'") cd vart

cdCommand ((Str arg):_) scr@(ScriptState out wd vart) = do
  fp  <- canonicalizePath arg
  workd <- canonicalizePath wd
  abs <- doesDirectoryExist fp
  case abs of
    True -> return $ if (workd/=fp) then 
            (ScriptState ("Directory changed to '"++fp++"'") fp vart) else 
             (ScriptState ("Directory not changed.") fp vart)
    False -> return $ ScriptState ("Directory '"++fp++"' not found.") wd vart 

-- COMMAND PWD
pwdCommand :: [Expr] -> ScriptState -> IO ScriptState
pwdCommand args scr@(ScriptState out wd vart) =  do
  return $ ScriptState wd wd vart



-- COMMAND CAT
catCommand :: [Expr] -> ScriptState -> IO ScriptState
catCommand [] (ScriptState out wd vart) = do
  return $ ScriptState "No argument(s) (file names) given." wd vart
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
    False -> do let sserr = ScriptState (out++
                                          "\n"++"File with name '"++
                                          arg++"' not found") wd vart
                case (length as > 0) of
                  True  -> lsCommand as sserr
                  False -> return sserr
catCommand _ scr = return scr



{-
flags :: M.Map String Flag
flags = M.fromList 
           [ ("-v" , vFlag )
           , ("-i" , iFlag )
           , ("-o" , oFlag )
           , ("-n" , nFlag )
           , ("-c" , cFlag )
           ]
-}
type Flag = String -> String -> String

-- COMMAND GREP
grepCommand :: [Expr] -> ScriptState -> IO ScriptState
grepCommand [] (ScriptState out wd vart) = do
  return $ ScriptState "No arguments (string and file name) given." wd vart

grepCommand [_] (ScriptState out wd vart) = do
  return $ ScriptState "No argument (file name) given." wd vart

--grepCommand (str:fn:flags) scr@(ScriptState out wd vart) = do
--  fn <- readFile fn
--  let 
        
-- COMMAND MV
mvCommand :: [Expr] -> ScriptState -> IO ScriptState
mvCommand [] (ScriptState out wd vart) = do
  return $ ScriptState "No arguments (file or directory names) given." wd vart
mvCommand [_] (ScriptState out wd vart) = do
  return $ ScriptState "No argument (file or directory name) given." wd vart
mvCommand args scr@(ScriptState out wd vart) = do
  src      <- canonicalizePath $ unwrap $ head args
  srcFilEx <- doesFileExist src
  srcDirEx <- doesDirectoryExist src

  dst      <- canonicalizePath $ unwrap $ last args
  dstFilEx <- doesFileExist dst
  dstDirEx <- doesDirectoryExist dst

  case srcFilEx of
    True -> do  case (hasExtension dst) of
                  True -> do 
                            outp <- fmap (intercalate "\n") $ mapM (\x -> mover (unwrap x) dst) $ init args
                            return $ ScriptState outp wd vart
                  False -> do
                             outp <- fmap (intercalate "\n") $ mapM (\x -> mover (unwrap x) (combine dst (getname (unwrap x)))) $ init args
                             return $ ScriptState outp wd vart
    False -> do  case (length args == 2) of
                   True -> do 
                             outp <- fmap (intercalate "\n") $ mapM (\x -> renameDir (unwrap x) dst) $ init args
                             return $ ScriptState outp wd vart
                   False-> do 
                             outp <- fmap (intercalate "\n") $ mapM (\x -> moverDir (unwrap x) dst) $ init args
                             return $ ScriptState outp wd vart
  where unwrap (Str str) = str
        getname x        = last $ S.splitOn "\\" x
        mover sr ds = do 
                        dfe <- doesFileExist sr
                        case (dfe) of
                          False -> return $ "Source file '"++sr++"' not found."
                          True  -> do
                                     copyFile sr ds
                                     removeFile sr 
                                     return $ "Command executed successfully."
        renameDir sr ds = do
                           dde <- doesDirectoryExist sr
                           case (dde) of
                             False -> return $ "Source directory '"++sr++"' not found."
                             True  -> do
                                        dirc <- getDirectoryContents wd 
                                        case (ds `elem` dirc) of
                                          False -> do 
                                                     renameDirectory sr ds
                                                     return $ "Command executed successfully."
                                          True  -> do
                                                     renameDirectory sr (ds++"_copy")
                                                     return $ "Command executed successfully."
                                        
        moverDir sr ds = do 
                        dde <- doesDirectoryExist sr
                        
                        case (dde) of
                          False -> return $ "Source directory '"++sr++"' not found."
                          True  -> do
                                     setCurrentDirectory ds
                                     copyDir sr ds
                                     removeDirectory sr 
                                     return $ "Command executed successfully."
        copyDir s d    = do
                           dirc <- getDirectoryContents s
                           browser dirc s
        browser [] src = return ()
        browser (fn:fs) src = do
          path <- canonicalizePath $ combine src $ fn
          dfe  <- doesFileExist path
          case (dfe) of
            True  -> copyFile path "."
            False -> do
                       createDirectory fn
                       setCurrentDirectory fn
                       dirl <- getDirectoryContents path
                       browser dirl path
                       

          

{-
          fnn <- canonicalizePath $ combine src $ fn
          putStrLn $ show $ fnn
          dfex <- doesFileExist fnn
          case (dfex) of
            True -> do 
                      _ <- copyFile fnn dest
                      browser fs src dest
            False -> do
                      _ <- createDirectory fnn
                      _ <- setCurrentDirectory fnn
                      desdirc <- getDirectoryContents src
                      browser desdirc dest fnn 
  -}                    
