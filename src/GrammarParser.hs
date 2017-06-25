module GrammarParser where

import Grammar

import Text.Megaparsec (some, space, oneOf, string, runParser, parse, char, newline, printChar, alphaNumChar, (<|>), sepBy1, sepEndBy1, try, parseErrorPretty)
import Text.Megaparsec.String (Parser)

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
    return $ NonTerminalName res

matchGrammar :: Grammar -> Grammar
matchGrammar (Grammar rules) = Grammar $ buildRules rules
    where buildRules ((Rule name (Production terms)):xs) = (Rule name (Production $ buildAlternativeTerms terms)):(buildRules xs)
          buildRules [] = []
          buildAlternativeTerms ((AlternativeTerm terms):xs) = (AlternativeTerm $ buildTerms terms):(buildAlternativeTerms xs)
          buildAlternativeTerms [] = []
          buildTerms (x:xs) = (buildTerm x):(buildTerms xs)
          buildTerms [] = []
          buildTerm (Many term) = Many $ buildPrimitive term
          buildTerm (Many1 term) = Many1 $ buildPrimitive term
          buildTerm (Primitive term) = Primitive $ buildPrimitive term
          buildPrimitive (NonTerminalName name) = findInRules rules name
          buildPrimitive a = a

findInRules :: [Rule] -> String -> PrimitiveTerm
findInRules (x:xs) name =
    let (Rule n _) = x in
    if name == n then NonTerminal x else findInRules xs name
findInRules [] name = undefined

parseFile :: String -> IO()
parseFile content = case (parse grammar "" content) of
                        Left err -> putStr (parseErrorPretty err)
                        Right x -> putStrLn $ show $ matchGrammar x
