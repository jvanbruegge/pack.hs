module Transform where

import AST (AST)
import qualified Typescript as T

transform :: AST -> AST
transform = T.transform
