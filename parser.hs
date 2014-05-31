import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

main :: IO()
main = do
  putStrLn ("I should parse something")

readExpr :: String -> String
readExpr input = case parse symbol "lisp" input of
  Left err -> "Could not find matching expression: " ++ show err
  Right val -> "Valid Value"
