module Hash( runScript, runInteractive) where

import Parsing.HashParser
import Language.Commands
import Language.Exec
import Language.Evals

import Control.Monad(void)
import qualified Data.Map as M
import qualified Data.List.Split as S

import System.Directory
import System.FilePath
import System.IO (hFlush, stdout)

import Data.Char (toLower)
import Language.Expressions

newVarTable :: VarTable
newVarTable = M.empty


-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript filePath = do
  cd <- getCurrentDirectory
  let script = ScriptState "" cd newVarTable
  file <- readFile $ "Scripts\\" ++ filePath
  let parsed =  customParse file
  void $ putStrLn $ "Here is the result of Hash script '"++ (takeFileName filePath) ++"':"
  case parsed of
    Left  e  -> putStrLn $ show $ e
    Right s  -> do 
                  (pr,ss) <- evaluate cd s script
                  return ()
                  runInteractive
-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
  cd <- getCurrentDirectory
  let prompt = cd
  interactive prompt $ ScriptState "" cd newVarTable
  

interactive :: String -> ScriptState -> IO ()
interactive prompt script@(ScriptState last currdir vart) = do
  setCurrentDirectory currdir
  putStr $ prompt ++ "> "
  hFlush stdout
  line <- getLine
  if (map toLower line) /= "quit" then do
    if (null line) then do interactive prompt script else do
      let parsed =  customParse line
      case parsed of
        Left  e  -> putStrLn $ show $ e
        Right s  -> do 
                      (pr,ss) <- evaluate prompt s script
                      interactive pr ss
  else do putStrLn $ "Thanks for using Hash. Bye!"
 

evaluate :: String -> [TLExpr] -> ScriptState -> IO (String,ScriptState)
evaluate prompt [] script = return (prompt,script)
evaluate prompt (tle:ts) script@(ScriptState last currdir vart) =
  case tle of
    TLCmd (Cmd "prompt" [Str p] _ _ _ ) -> do
      evaluate p ts script
    TLCmd (Cmd nm ar ind outd app) -> do
      case (M.member nm commands) of
        True -> do
                  let comm = unwrap $ M.lookup nm commands
                                                                   
                  ss <- comm ar script
                  case outd of
                    Nothing      -> putStrLn $ output ss  
                    Just (Str f) -> case app of
                                     False -> writeFile f $ output ss 
                                     True  -> appendFile f $ "\n" ++ output ss
                  let prmpt = if (nm == "cd") then (wd ss) else prompt
                  evaluate prmpt ts $ ScriptState "" (wd ss) (vartable ss)
        False -> do
                   let varch = evalCalc (Var nm) vart
                   case outd of
                     Nothing      -> putStrLn $ numton varch 
                     Just (Str f) -> case app of
                                     False -> writeFile f $ numton varch
                                     True  -> appendFile f $ "\n" ++ numton varch
                   evaluate prompt ts $ ScriptState (numton varch) currdir vart
                   
                   
    TLCmd (Assign (Var var) val)         -> do
      evaluate prompt ts $ ScriptState last currdir $ M.insert var (evalCalc val vart) vart
    TLCnd (If cnd cth) -> case (evalPredicate cnd vart) of
                            True -> evaluate prompt cth script
                            False -> evaluate prompt ts  script
    TLCnd (IfElse cnd cth cel) -> case (evalPredicate cnd vart) of
                            True -> evaluate prompt cth script
                            False -> evaluate prompt cel  script
    TLCnd wh@(While cnd cth) -> case (evalPredicate cnd vart) of
                            True  -> evaluate prompt (cth++(tle:ts)) script
                                       
                            False -> evaluate prompt ts  script
  where unwrap name@(Just g)  = g 
        unwrap name@(Nothing) = error "Command not found."
        numton (Num n)        = show n
        numton _              = "Command not found"

