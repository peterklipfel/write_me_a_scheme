import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List[LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = [first] ++ rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    otherwise -> Atom atom 

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ noneOf "\""
  char '"'
  return $ String x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "Could not find matching expression: " ++ show err
  Right val -> "Valid Value"

showVal :: LispVal -> String
showVal (String contents) = "\""++contents++"\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

spaces :: Parser()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


main :: IO()
main = do
  args <- getArgs
  putStrLn . readExpr $ args !! 0

