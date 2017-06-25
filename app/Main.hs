module Main where

import GrammarParser (parseFile)

file :: String
file = "languages/typescript.abnf"

main :: IO ()
main = readFile file >>= parseFile file
