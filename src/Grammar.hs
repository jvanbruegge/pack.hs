module Lexer where

test :: String
test = "function test4(peter :number, jsd: number, jsdas, sda : string) : boolean {"
    ++ "return peter === jsd + jsdas;"
    ++ "}"

newtype Grammar = Grammar [Rule]
data Rule = Rule String Production
newtype Production = Production [ProductionPart]
data ProductionPart = Terminal String
                    | NonTerminal Rule
                    | Optional Production
                    | Alternative Production Production
                    | Group [Production]
                    | Many Production
                    | Many1 Production
                    | Repetition Int Production
                    | Identifier
                    | Literal

join :: Show a => String -> [a] -> String
join _ [] = ""
join _ (t:[]) = show t
join char (h:t) = show h ++ char ++ join char t

instance Show Grammar where
    show (Grammar l) = join "\n" l

instance Show Rule where
    show (Rule name prod) = name ++ " ::= " ++ show prod

instance Show Production where
    show (Production l) = join " " l

instance Show ProductionPart where
    show (Terminal s) = "'" ++ s ++ "'"
    show (NonTerminal (Rule n _)) = n
    show (Optional p) = "[" ++ show p ++ "]"
    show (Alternative a b) = show a ++ " | " ++ show b
    show (Group l) = "(" ++ join " " l ++ ")"
    show (Many p) = show p ++ "*"
    show (Many1 p) = show p ++ "+"
    show (Repetition n p) = show n ++ "*" ++ show p
    show Identifier = "IDENTIFIER"
    show Literal = "LITERAL"

typescript :: Grammar
typescript = Grammar [
        Rule "S" $ Production [NonTerminal function],
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


function = Rule "function" $ Production [
        Terminal "function",
        Identifier,
        Terminal "(",
        Optional $ Production [NonTerminal arguments],
        Terminal ")",
        Optional $ Production [NonTerminal tsTypeDeclaration],
        NonTerminal block
    ]

arguments = Rule "arguments" $ Production [
        NonTerminal argument,
        Optional $ Production [NonTerminal additionalArguments]
    ]

additionalArguments = Rule "additionalArguments" $ Production [
        Terminal ",",
        NonTerminal argument,
        Optional $ Production [NonTerminal additionalArguments]
    ]

argument = Rule "argument" $ Production [
        Identifier,
        Optional $ Production [NonTerminal tsTypeDeclaration]
    ]

block = Rule "block" $ Production [
        Terminal "{",
        Many $ Production [NonTerminal statement],
        Terminal "}"
    ]

tsType = Rule "type" $ Production [
    Alternative
        (Production [Alternative
            (Production [Terminal "boolean"])
            (Production [Terminal "string"])
        ])
        (Production [Terminal "number"])
    ]

tsTypeDeclaration = Rule "typeDeclaration" $ Production [
        Terminal ":",
        NonTerminal tsType
    ]

statement = Rule "statement" $ Production [
        Terminal "return",
        NonTerminal expression,
        Optional $ Production [Terminal ";"]
    ]

literal = Rule "literal" $ Production [
    Alternative
        (Production [Alternative
            (Production [Terminal "\"", Literal, Terminal "\""])
            (Production [Literal])
        ])
        (Production [Alternative
            (Production [Terminal "true"])
            (Production [Terminal "false"])
        ])
    ]

expression = Rule "expression" $ Production [
    Alternative
        (Production [Alternative
            (Production [Identifier])
            (Production [NonTerminal literal])
        ])
        (Production [
            NonTerminal expression,
            NonTerminal operator,
            NonTerminal expression
        ])
    ]

operator = Rule "operator" $ Production [
    Alternative
        (Production [Alternative
            (Production [Terminal "+"])
            (Production [Terminal "-"])
        ])
        (Production [Terminal "==="])
    ]
