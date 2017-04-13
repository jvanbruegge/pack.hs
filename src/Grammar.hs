module Lexer where

test :: String
test = "function test4(peter :number, jsd: number, jsdas, sda : string) : boolean {"
    ++ "return peter === jsd + jsdas;"
    ++ "}"

newtype Grammar = Grammar [Rule]
data Rule = Rule String [Production]
newtype Production = Production [ProductionPart]
data ProductionPart = Terminal String
                    | OptionalTerminal String
                    | Identifier
                    | Literal
                    | Epsilon
                    | SubProd Rule
                    | SubProdList Rule
                    | OptionalProd Rule

join :: Show a => String -> [a] -> String
join _ [] = ""
join char (h:t) =
    let helper [] = ""
        helper (x:xs) = char ++ show x ++ helper xs
    in show h ++ helper t

instance Show Grammar where
    show (Grammar l) = join "\n" l

instance Show Rule where
    show (Rule name prods) = name ++ " ::= " ++ join " | " prods

instance Show Production where
    show (Production l) = join " " l

instance Show ProductionPart where
    show (Terminal s) = "'" ++ s ++ "'"
    show (OptionalTerminal s) = "['" ++ s ++ "']"
    show Identifier = "IDENTIFIER"
    show Literal = "LITERAL"
    show Epsilon = "EPSILON"
    show (SubProd (Rule name _)) = name
    show (SubProdList (Rule name _)) = "{" ++ name ++ "}"
    show (OptionalProd (Rule name _)) = "[" ++ name ++ "]"

typescript :: Grammar
typescript = Grammar [
        Rule "S" [ (Production [SubProd function]) ],
        function,
        arguments,
        additionalArguments,
        argument,
        block,
        tsType,
        tsTypeDeclaration,
        statement,
        literal,
        expression,
        operator
    ]


function = Rule "function" [
        (Production [Terminal "function", Identifier, Terminal "(", OptionalProd arguments, Terminal ")", OptionalProd tsTypeDeclaration, SubProd block])
    ]

arguments = Rule "arguments" [
        (Production [SubProd argument, OptionalProd additionalArguments])
    ]

additionalArguments = Rule "additionalArguments" [
        (Production [Terminal ",", SubProd argument, OptionalProd additionalArguments])
    ]

argument = Rule "argument" [
        (Production [Identifier, SubProd tsTypeDeclaration]),
        (Production [Identifier])
    ]

block = Rule "block" [
        (Production [Terminal "{", SubProdList statement, Terminal "}"])
    ]

tsType = Rule "type" [
        (Production [Terminal "boolean"]),
        (Production [Terminal "string"]),
        (Production [Terminal "number"])
    ]

tsTypeDeclaration = Rule "typeDeclaration" [
        (Production [Terminal ":", SubProd tsType])
    ]

statement = Rule "statement" [
        (Production [Terminal "return", SubProd expression, OptionalTerminal ";"])
    ]

literal = Rule "literal" [
        (Production [Terminal "\"", Literal, Terminal "\""]),
        (Production [Literal]),
        (Production [Terminal "true"]),
        (Production [Terminal "false"])
    ]

expression = Rule "expression" [
        (Production [Identifier]),
        (Production [SubProd literal]),
        (Production [SubProd expression, SubProd operator, SubProd expression])
    ]

operator = Rule "operator" [
        (Production [Terminal "+"]),
        (Production [Terminal "-"]),
        (Production [Terminal "==="])
    ]
