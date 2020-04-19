module Main where
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

--parseString :: Parser LispVal
--parseString = do
--                char '"'
--                x <- many (noneOf "\"")
--                char '"'
--                return $ String x

--Exercise 2.1-3
parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many (string "\\\"" <|> noneOf "\"")
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first : rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

--Exercise 2.1-1
--parseNumber :: Parser LispVal
--parseNumber = do
--                digitStr <- many1 digit
--                return $ Number (read digitStr)

--Exercise 2.1-2
--parseNumber :: Parser LispVal
--parseNumber = (many1 digit) >>= (return . Number . read)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
