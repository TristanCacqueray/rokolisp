-- | The language evaluator
module RokoLisp.Eval (Term (..)) where

import Relude

type Name = Text

-- | The Term data type
data Term
  = Var Name
  | Lam Name Term
  | App Term Term
  deriving stock (Show, Eq)
