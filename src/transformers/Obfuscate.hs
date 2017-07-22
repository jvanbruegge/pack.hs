module Obfuscate where

import Data.Tuple (fst)
import Data.List ((!!), replicate, foldl, concat, permutations)
import Data.Monoid (mappend)
import Control.Monad (mapM, replicateM, (>>=))
import Control.Monad.State.Lazy (State, evalState, get, put)

import AST (GeneralAST(..), AST)

letters :: [Char]
letters = ['a'..'z'] `mappend` ['A'..'Z'] `mappend` ['$']

alphaNum :: [Char]
alphaNum =  letters `mappend` ['0'..'9']

names :: [String]
names = [ x:xs | i <- [0..], x <- letters, xs <- replicateM i alphaNum ]

symbols :: [String]
symbols = ["!", "\"", "%", "&", "/", "(", ")", "=", "?", "*", "+", "~", "'", "{", "}", "[", "]", ".", ":", ",", ";"]

transform :: AST -> AST
transform a = evalState (helper a) names
    where helper :: AST -> State [String] AST
          helper (Node "function" (x:y:xs)) = do
              n:t <- get
              put t
              x' <- helper x
              xs' <- mapM helper xs
              return $ Node "function" $ x':(Leaf n):xs'
          helper (Node s l) = mapM helper l >>= return . (Node s)
          helper a = return a

generateOutput :: AST -> String
generateOutput a = foldl (++) "" $ intersparseStrings $ reverse $ flattenLeafs [] $ transform a
    where intersparseStrings :: [String] -> [String]
          intersparseStrings (x:y:xs) = if x `elem` symbols || y `elem` symbols
            then x:(intersparseStrings (y:xs))
            else x:" ":(intersparseStrings (y:xs))
          intersparseStrings a = a
          flattenLeafs x (Node _ l) = foldl flattenLeafs x l
          flattenLeafs x (Leaf s) = s:x
