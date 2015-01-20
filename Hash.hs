module Hash( runScript, runInteractive) where

import Parsing.HashParser
import Language.Exec
import Control.Monad(void)
import qualified Data.Map as M
import System.Directory
import System.FilePath
import System.IO (hFlush, stdout)

import Data.Char (toLower)


newVarTable :: VarTable
newVarTable = M.empty


-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript filePath = do
  let scriptState = ScriptState "test" getCurrentDirectory newVarTable
  file <- readFile $ "Scripts\\" ++ filePath
  let a =  customParse file
  void $ putStrLn $ "Here is the result of Hash script '"++ (takeFileName filePath) ++"':"
  case a of
    Left  e  -> putStrLn $ show $ e
    Right s  -> putStrLn $ show s
-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = do
  interactive $ ScriptState "test" getCurrentDirectory newVarTable
 

interactive :: ScriptState -> IO ()
interactive scriptState@(ScriptState last currdir varT) = do
  cd <- currdir
  setCurrentDirectory cd
  putStr $ cd++"> "
  hFlush stdout
  line <- getLine
  if (map toLower line) /= "quit" then do
    if (null line) then do runInteractive else do
      let parsed =  customParse line
      case parsed of
        Left  e  -> putStrLn $ show $ e
        Right s  -> putStrLn $ show s
      interactive scriptState 
  
  else do putStrLn $ "Thanks for using Hash. Bye!"




