module Typescript where

import AST (GeneralAST(..), AST)

transform :: AST -> AST
transform (Node s l) = Node s $ fmap transform $ filter syntaxTypescript l
    where syntaxTypescript (Node "typeDeclaration" _) = False
          syntaxTypescript _ = True
transform a = a
