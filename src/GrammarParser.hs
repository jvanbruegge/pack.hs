module GrammarParser where

import Grammar

import Text.Megaparsec (some, space, string, runParser, parseTest, char, printChar, alphaNumChar, (<|>), sepBy1, sepEndBy1)
import Text.Megaparsec.String

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
term = terminal
--    <|> nonTerminal
    <|> optional
--    <|> group
--    <|> many
    <|> identifier
    <|> literal

terminal :: Parser Term
terminal = do
    char '\''
    s <- some alphaNumChar
    char '\''
    return $ Terminal s

optional :: Parser Term
optional = do
    char '['
    res <- alternativeTerm
    char ']'
    return $ Optional res

identifier :: Parser Term
identifier = string "IDENTIFIER" >> return Identifier

literal :: Parser Term
literal = string "LITERAL" >> return Literal

parseFile :: String -> IO ()
parseFile content = parseTest rule content
