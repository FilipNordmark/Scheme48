module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

main :: IO ()
main = putStrLn =<< readExpr . head <$> getArgs

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = () <$ many space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val



--------------------------------------------------------------------------------
--Return Values

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float  Float
             | String String
             | Bool Bool
    deriving (Show, Eq, Ord)


parseExpr :: Parser LispVal
parseExpr = parseAtom 
        <|> parseString 
        <|> try parseFloat 
        <|> parseNumber
        <|> parseQuoted
        <|> char '(' *> (try parseList <|> parseDottedList) <* char ')'

parseString :: Parser LispVal
parseString = String <$> (char '"' *> many (char '\\' *> oneOf escape <|> noneOf "\"") <* char '"')
    where escape = "nrt\\\""


parseAtom :: Parser LispVal
parseAtom = do 
    atom <- (:) <$> (letter <|> symbol) <*> many (letter <|> digit <|> symbol)
    return $ case atom of 
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseFloat :: Parser LispVal
parseFloat = do
    f <- many digit
    r <- (:) <$> char '.' <*> many1 digit
    pure $ Float (read (f ++ r))

parseList :: Parser LispVal
parseList = List <$> parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- parseExpr `endBy` spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    q <- oneOf "\'`,"
    let quote = case q of {'\'' -> "quote"; '`' -> "quasiquote"; ',' -> "unquote"}
    x <- parseExpr
    return $ List [Atom quote, x]


--------------------------------------------------------------------------------
-- Beginning the Evaluator


showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

