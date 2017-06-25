module Main where

import GrammarParser (parseFile)

main :: IO ()
main = readFile "languages/typescript.abnf" >>= parseFile
