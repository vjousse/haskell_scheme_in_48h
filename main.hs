module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

main :: IO()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))

data LispVal = Atom String
             | List [LispVal]
             | DottedLisp [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- Second solution with do notation
-- parseNumber = do digits <- many1 digit
--                  return $ (Number .read) $ digits

-- Third solution with bind operator >>=
-- parseNumber = many1 digit >>= return . (Number . read)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"")
                 char '"'
                 return $ String x


parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space
