module Grammar where

newtype Grammar = Grammar [Rule]
data Rule = Rule String Production
newtype Production = Production [AlternativeTerm]
newtype AlternativeTerm = AlternativeTerm [Term]
data Term = Terminal String
          | NonTerminal Rule
          | Optional AlternativeTerm
          | Group Production
          | Many AlternativeTerm
          | Many1 AlternativeTerm
          | Repetition Int Term
          | Identifier
          | Literal

join :: Show a => String -> [a] -> String
join _ [] = ""
join _ (t:[]) = show t
join char (h:t) = show h ++ char ++ join char t

instance Show Grammar where
    show (Grammar l) = join "\n" l

instance Show Rule where
    show (Rule name prod) = name ++ " ::= " ++ show prod ++ ";"

instance Show Production where
    show (Production l) = join " | " l

instance Show AlternativeTerm where
    show (AlternativeTerm l) = join " " l

instance Show Term where
    show (Terminal s) = "'" ++ s ++ "'"
    show (NonTerminal (Rule n _)) = n
    show (Optional p) = "[" ++ show p ++ "]"
    show (Group l) = "(" ++ show l ++ ")"
    show (Many p) = show p ++ "*"
    show (Many1 p) = show p ++ "+"
    show (Repetition n p) = show n ++ "*" ++ show p
    show Identifier = "IDENTIFIER"
    show Literal = "LITERAL"
