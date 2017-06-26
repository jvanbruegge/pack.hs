module GrammarParser where

import Grammar

import Text.Megaparsec (some, space, oneOf, string, runParser, parse, char, newline, printChar, alphaNumChar, (<|>), sepBy1, sepEndBy1, try, parseErrorPretty)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Error (ParseError, Dec)
import Text.Megaparsec.Prim (Token)

grammar :: Parser Grammar
grammar = sepEndBy1 rule (some newline) >>= return . Grammar

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
        <|> nonTerminal

terminal :: Parser PrimitiveTerm
terminal = do
    char '\''
    s <- terminalLiteral
    char '\''
    return $ Terminal s

terminalLiteral :: Parser String
terminalLiteral = some (
            try (string "\\'")
        <|> try (string "\\\\")
        <|> (alphaNumChar >>= return . (:[]))
        <|> (oneOf ['!', '"', '§', '$', '%', '&', '/', '(', ')', '=', '?', '{', '}', '[', ']', '`', '´', '+', '*', '~', '#', ',', ';', '.', ':', '-', '_', '@', ' '] >>= return . (:[]))
        ) >>= return . concat

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

nonTerminal :: Parser PrimitiveTerm
nonTerminal = do
    res <- some alphaNumChar
    space
    return $ NonTerminal res

parseFile :: String -> String -> Either String Grammar
parseFile filename content = case (parse grammar filename content) of
         Left err -> Left $ parseErrorPretty err
         Right xs -> Right xs
