module Main where

import Lib
import GrammarParser

main :: IO ()
main = readFile "languages/test.abnf" >>= parseFile
