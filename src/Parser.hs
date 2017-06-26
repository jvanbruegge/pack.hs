module Parser where

import Grammar
import GrammarParser (terminalLiteral)

import Text.Megaparsec (parse, parseErrorPretty, some, many, (<|>), string, alphaNumChar, char, space, option)
import Text.Megaparsec.String (Parser)

import System.FilePath (FilePath)
import Debug.Trace

data AST = Node String [AST] deriving (Show)

generateParser :: Grammar -> Parser AST
generateParser grammar = parseRule (getRule grammar "S") grammar

getRule :: Grammar -> String -> Rule
getRule (Grammar rules) name = head $ filter (\(Rule n _) -> n == name) rules

parseRule :: Rule -> Grammar -> Parser AST --TODO: Maybe Reader monad?
parseRule (Rule _ (Production terms)) g = generateAlternatives terms g

generateAlternatives :: [AlternativeTerm] -> Grammar -> Parser AST
generateAlternatives terms grammar = foldl (<|>) (helper grammar (head terms)) $ map (helper grammar) (tail terms)
    where helper grammar (AlternativeTerm terms) = sequence (generateTerms grammar terms) >>= return . (Node "AlternativeTerm")

generateTerms :: Grammar -> [Term] -> [Parser AST]
generateTerms grammar = map $ helper grammar
    where helper grammar x = do
              res <- parser grammar x
              space
              return res
          parser grammar x = case x of
              Many p -> (many $ generatePrimitive p grammar) >>= return . (Node "Many")
              Many1 p -> (some $ generatePrimitive p grammar) >>= return . (Node "Many1")
              Primitive p -> generatePrimitive p grammar

generatePrimitive :: PrimitiveTerm -> Grammar -> Parser AST
generatePrimitive (Terminal s) _ = string s >>= \s -> return $ Node s []
generatePrimitive (Optional (Production terms)) grammar = option (Node "" []) $ generateAlternatives terms grammar
generatePrimitive (Group (Production terms)) grammar = generateAlternatives terms grammar
generatePrimitive (Repetition n term) grammar = (sequence $ take n $ repeat $ generatePrimitive term grammar) >>= return . (Node "Repetition")
generatePrimitive Identifier _ = some alphaNumChar >>= \s -> return $ Node s []
generatePrimitive Literal _ = do
                      s <- some alphaNumChar
                      return $ Node s []
generatePrimitive (NonTerminal name) grammar = parseRule (getRule grammar name) grammar

parseFile :: Grammar -> FilePath -> String -> Either String AST
parseFile g f content = case (parse (generateParser g) f content) of
        Left err -> Left $ parseErrorPretty err
        Right x -> Right x
