import System.Environment
import Hash
import Control.Monad (void)

main :: IO()
main = do
  args <- getArgs
  void $ putStr $ "Welcome to Hash "
  void $ putStrLn $ "(simple Shell emulator written in Haskell)."
  if null args then do
    putStrLn $ "You may enter commands!"
    runInteractive 
  else do 
   let ha = head args
   runScript ha


