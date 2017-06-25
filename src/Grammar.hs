module Grammar where

newtype Grammar = Grammar [Rule]
data Rule = Rule String Production
newtype Production = Production [AlternativeTerm]
newtype AlternativeTerm = AlternativeTerm [Term]
data Term = Many PrimitiveTerm
          | Many1 PrimitiveTerm
          | Primitive PrimitiveTerm
data PrimitiveTerm = Terminal String
                   | NonTerminal Rule
                   | NonTerminalName String --Internal only
                   | Optional Production
                   | Group Production
                   | Repetition Int PrimitiveTerm
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
    show (Many p) = show p ++ "*"
    show (Many1 p) = show p ++ "+"
    show (Primitive p) = show p

instance Show PrimitiveTerm where
    show (Terminal s) = "'" ++ s ++ "'"
    show (NonTerminal (Rule n _)) = n
    show (NonTerminalName _) = undefined
    show (Optional p) = "[" ++ show p ++ "]"
    show (Group l) = "(" ++ show l ++ ")"
    show (Repetition n p) = show n ++ "*" ++ show p
    show Identifier = "IDENTIFIER"
    show Literal = "LITERAL"
