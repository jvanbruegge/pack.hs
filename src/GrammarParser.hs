module GrammarParser where

import Grammar
import Debug.Trace

import Text.Megaparsec (some, space, string, runParser, parseTest, char, printChar, alphaNumChar, (<|>), sepBy1, sepEndBy1, try)
import Text.Megaparsec.String (Parser)

rule :: Parser Rule
rule = do
    name <- some alphaNumChar
    space
    string "::="
    space
    prod <- production
    char ';'
    return $ Rule name prod

production :: Parser Production
production = sepBy1 alternativeTerm (char '|') >>= return . Production

alternativeTerm :: Parser AlternativeTerm
alternativeTerm = space >> sepEndBy1 term space >>= return . AlternativeTerm

term :: Parser Term
term = (try many)
   <|> (try many1)
   <|> (primitiveTerm >>= return . Primitive)

primitiveTerm :: Parser PrimitiveTerm
primitiveTerm = terminal
        <|> identifier
        <|> literal
        <|> optional
        <|> group
--    <|> nonTerminal

terminal :: Parser PrimitiveTerm
terminal = do
    char '\''
    s <- some alphaNumChar
    char '\''
    return $ Terminal s

optional :: Parser PrimitiveTerm
optional = do
    char '['
    res <- production
    char ']'
    return $ Optional res

group :: Parser PrimitiveTerm
group = do
    char '('
    res <- production
    char ')'
    return $ Group res

identifier :: Parser PrimitiveTerm
identifier = string "IDENTIFIER" >> return Identifier

literal :: Parser PrimitiveTerm
literal = string "LITERAL" >> return Literal

many :: Parser Term
many = do
    res <- primitiveTerm
    char '*'
    return $ Many res

many1 :: Parser Term
many1 = do
    res <- primitiveTerm
    char '+'
    return $ Many1 res

parseFile :: String -> IO ()
parseFile content = parseTest rule content
