module AST where

import Data.List (intercalate)

data GeneralAST a = Node a [GeneralAST a]
         | Leaf a
         | Empty
        deriving (Show)

type AST = GeneralAST String

instance Functor GeneralAST where
    fmap f (Node s l) = Node (f s) $ fmap (fmap f) l
    fmap f (Leaf s)   = Leaf (f s)
    fmap f Empty      = Empty

prettyPrint :: AST -> String
prettyPrint a = helper "" a
    where helper indent (Node s l) = indent ++ "Node \"" ++ s ++ "\" [\n"
                ++ (intercalate ",\n" $ fmap (helper (indent ++ "    ")) l) ++ "\n"
                ++ indent ++ "]"
          helper indent a = indent ++ show a

pruneEmpty :: GeneralAST a -> GeneralAST a
pruneEmpty (Node s l) = Node s $ fmap pruneEmpty $ filter notEmpty l
    where notEmpty Empty = False
          notEmpty _ = True
pruneEmpty a = a

extractList :: [String] -> AST -> AST
extractList n (Node s l) = Node s $ concat $ fmap (helper n) l
    where helper n (Node name ll) = if name `elem` n
                then fmap (extractList n) ll
                else [extractList n (Node name ll)]
          helper n a = [extractList n a]
extractList n a = a

preprocess :: AST -> AST
preprocess = extractList ["Optional", "Many", "Many1", "Group", "Repetition"] . pruneEmpty
