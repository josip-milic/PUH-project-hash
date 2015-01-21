-- Contains the parsers that take a string and produce an executable list of
-- TLExpr constructs. We recommend Parsec for parsing.
module Parsing.HashParser (topParser, customParse) where

import System.FilePath
import System.Directory
import System.Environment

import Text.Parsec.String (Parser)
import Text.Parsec.Char (digit,char,spaces,string,oneOf,noneOf)
import Text.Parsec (parse, ParseError)
import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>),(<|>),many, Applicative)
import Data.List (intercalate) 
import Data.Char (digitToInt)
import Text.Parsec.Combinator (many1)
import Control.Monad (void, join)

import Text.Parsec.Combinator (sepBy1)
import Text.Parsec.Char (letter)
import Text.Parsec.Char (alphaNum)
import Text.Parsec.Char (anyChar)
import Text.Parsec.Expr
import Text.Parsec (try)

import qualified Data.List.Split as S

import qualified Data.Map as M



customParse :: String -> Either ParseError String
customParse line = parse topParser "Parse error" line 

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

strip :: String -> Parser ()
strip str = void $ lexeme $ irrelevant  *> string str

irrelevant :: Parser ()
irrelevant = try commentParser <|> try whitespace <|> irrelevant

lexeme :: Parser a -> Parser a
lexeme p = p <* irrelevant

symbol :: String -> Parser String
symbol s = lexeme $ string s

variable :: Parser String
variable = do
  l <- lexeme $ irrelevant  *> letter
  rest <- many alphaNum
  return $ l:rest  
  

--parens :: Parser String
--parens = between (symbol "(") (symbol ")") simpleExpr

condTrueParser :: Parser String
condTrueParser = do
  strip "True"
  return "True"

condFalseParser :: Parser String
condFalseParser = do
  strip "False"
  return "False"
  
condParser :: Parser String
condParser = try condTrueParser <|> condFalseParser

topParser :: Parser String
topParser = try ifBlock <|> commandParser  

echoCommParser = try echoStrCommParser <|> echoVarCommParser 

echoVarCommParser :: Parser String 
echoVarCommParser = do
  strip "echo"
  stringReplaceParser

stringReplaceParser :: Parser String
stringReplaceParser = do
    var <- fmap evalExpression expression
    case var of
      Just (Num n)     -> return $ show n
      Just (Boolean b) -> return $ show b
      Nothing          -> return "${Value not recognized.}"

stringEveryParser :: Parser String
stringEveryParser = do
  w <- many $ noneOf "\""
  return w

stringParser = try stringReplaceParser <|> stringEveryParser
  

commentParser :: Parser ()
commentParser = do
  void $ lexeme $ whitespace *> string "#"
  w <- many $ noneOf "\n"
  _ <- void $ string "\n"
  void $ irrelevant

echoStrCommParser :: Parser String 
echoStrCommParser = do
  strip "echo"
  strip "\""
  str <- many $ noneOf "\""
  strip "\""
  return $ intercalate " " $ f $ S.splitOn " " str
  where f l = fmap (unwrap . (parse stringParser "err")) l
        unwrap (Right st) = st
  
assignCommParser :: Parser String
assignCommParser = do
  v <- variable
  strip "="
  e <- fmap evalExpression expression
  return $ v ++ ": " ++ (show e)
  


commandParser :: Parser String
commandParser = try echoCommParser <|> assignCommParser

ifThen :: Parser String
ifThen = do
  strip "if"
  cond <- fmap evalComparison comparison
  strip "then"
  strip "("
  parsed <- topParser
  strip ")"
  case cond of
    Just True  -> return parsed
    Just False -> return ""
    Nothing    -> return "Condition not set right."

ifThenElse :: Parser String
ifThenElse = do
  strip "if"
  cond <- fmap evalComparison comparison
  strip "then"
  strip "("
  parsed1 <- topParser
  strip ")"
  strip "else"
  strip "("
  parsed2 <- topParser
  strip ")"
  case cond of
    Just True  -> return parsed1
    Just False -> return parsed2
    Nothing    -> return "Condition not set right."

ifBlock :: Parser String
ifBlock = try ifThenElse <|> ifThen


-- A bottom-level expression
data Expr = Var String -- A named variable
          | Num Integer 
          | Boolean Bool
          | Str String -- A mere string, the peasant of expressions
          deriving (Eq, Show)

-- A comparison operation
data Comp = CEQ Expr Expr -- ==
          | CNE Expr Expr -- /=
          | CGE Expr Expr -- >=
          | CGT Expr Expr -- >
          | CLE Expr Expr -- <=
          | CLT Expr Expr -- <
          | CLI Expr -- A wrapped expression literal - True if nonempty
          deriving (Eq, Show)

type VarTable = M.Map String String


expressionTrue :: Parser Expr
expressionTrue = do
  strip "True"
  return $ Boolean $ True

expressionFalse :: Parser Expr
expressionFalse = do
  strip "False"
  return $ Boolean $ False

expressionVar :: Parser Expr
expressionVar = do
  strip "$"
  l <- letter
  rest <- many alphaNum
  return $ Var $ l:rest  


expressionNum :: Parser Expr
expressionNum = do
  d <- lexeme $ irrelevant  *>  many1 digit
  return $ Num $ read d  

expression :: Parser Expr
expression = try expressionVar   <|> 
             try expressionNum   <|>
             try expressionTrue  <|> 
             expressionFalse     


comparisonEQ :: Parser Comp
comparisonEQ = do
  expr1 <- expression
  strip "=="
  expr2 <- expression
  return $ CEQ expr1 expr2

comparisonNE :: Parser Comp
comparisonNE = do
  expr1 <- expression
  strip "/="
  expr2 <- expression
  return $ CNE expr1 expr2

comparisonGE :: Parser Comp
comparisonGE = do
  expr1 <- expression
  strip ">="
  expr2 <- expression
  return $ CGE expr1 expr2

comparisonGT :: Parser Comp
comparisonGT = do
  expr1 <- expression
  strip ">"
  expr2 <- expression
  return $ CGT expr1 expr2

comparisonLE :: Parser Comp
comparisonLE = do
  expr1 <- expression
  strip "<="
  expr2 <- expression
  return $ CLE expr1 expr2

comparisonLT :: Parser Comp
comparisonLT = do
  expr1 <- expression
  strip "<"
  expr2 <- expression
  return $ CLT expr1 expr2

comparisonLI :: Parser Comp
comparisonLI = do
  expr <- expression
  return $ CLI expr

--varTable = M.insert "a" "b" M.empty
--M.lookup "k" varTable
--varTable = M.empty

varTable :: M.Map String Expr
varTable = M.insert "b" (Boolean True) $ M.insert "a" (Num 2) M.empty


comparison :: Parser Comp
comparison = try comparisonEQ <|> 
             try comparisonNE <|> 
             try comparisonGE <|> 
             try comparisonLE <|> 
             try comparisonGT <|> 
             try comparisonLT <|> 
             comparisonLI


evalExpression :: Expr -> Maybe Expr
evalExpression p = case p of
  Num n     -> Just $ Num n
  Boolean b -> Just $ Boolean b
  Var v     -> M.lookup v varTable



evalComparison :: Comp -> Maybe Bool
evalComparison p = case p of
  CEQ e1 e2 -> eval (==) (evalExpression e1) (evalExpression e2)
  CNE e1 e2 -> eval (/=) (evalExpression e1) (evalExpression e2)
  CGE e1 e2 -> eval (>=) (evalExpression e1) (evalExpression e2)
  CGT e1 e2 -> eval (>)  (evalExpression e1) (evalExpression e2)
  CLE e1 e2 -> eval (<=) (evalExpression e1) (evalExpression e2)
  CLT e1 e2 -> eval (<)  (evalExpression e1) (evalExpression e2)
  CLI e     -> eval' (evalExpression e)
  where eval  _   (Just (Boolean _ ))  _                  = Nothing 
        eval  _   _                    (Just (Boolean _)) = Nothing
        eval  _   _                    Nothing            = Nothing
        eval  _   Nothing              _                  = Nothing
        eval  f   (Just (Num n1))      (Just (Num n2))    = Just $ f n1 n2 

        eval' (Just (Boolean b)) = Just b
        eval' _                  = Nothing


        


                 



