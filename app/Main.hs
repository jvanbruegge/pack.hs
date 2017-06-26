module Main where

import qualified GrammarParser as G (parseFile)
import qualified Parser as P (parseFile)

grammarFile :: String
grammarFile = "languages/typescript.abnf"

sourceFile :: String
sourceFile = "demo/test.ts"

main :: IO ()
main = do
    sourceCode <- readFile sourceFile
    grammar <- readFile grammarFile >>= return . (G.parseFile grammarFile)
    let message = case grammar of
            Left err -> err
            Right x -> case (P.parseFile x sourceFile sourceCode) of
                    Left err -> err
                    Right y -> show y
    putStrLn message
