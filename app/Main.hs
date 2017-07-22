module Main where

import AST (prettyPrint)
import Transform (transform)
import qualified GrammarParser as G (parseFile)
import qualified Parser as P (parseFile)
import qualified Obfuscate as O (generateOutput)

grammarFile :: String
grammarFile = "languages/typescript.abnf"

sourceFile :: String
sourceFile = "demo/test.ts"

extractMessage grammar sourceFile sourceCode = case grammar of
        Left err -> err
        Right x -> case (P.parseFile x sourceFile sourceCode) of
                Left err -> err
                Right y -> O.generateOutput $ transform y


main :: IO ()
main = do
    sourceCode <- readFile sourceFile
    grammar <- readFile grammarFile >>= return . (G.parseFile grammarFile)
    let result = extractMessage grammar sourceFile sourceCode
    putStrLn result
    writeFile "out/test.js" result
