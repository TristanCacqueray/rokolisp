-- | The language evaluator
module RokoLisp.Eval
  ( Term (..),
    Value (..),
    Literal (..),
    alphaConvert,
    betaReduce,
    eval,
  )
where

import Data.Map (lookup)
import qualified Data.Text as Text
import Relude
import qualified Text.Show

type Name = Text

-- | The Term data type
data Term
  = Var Name
  | Lam Name Term
  | App Term Term
  deriving stock (Show, Eq)

-- | Convert conflicting variable name (based on code by Renzo Carbonara)
--
-- >>> alphaConvert "f" (Var "x") (Lam "x" (App (Var "f") (Var "x")))
-- Lam "xx" (App (Var "x") (Var "xx"))
alphaConvert :: Name -> Term -> Term -> Term
alphaConvert name term = \case
  Var varName -> if name == varName then term else Var varName
  App t1 t2 -> App (convert t1) (convert t2)
  Lam varName body
    | -- varName shadows name
      name == varName ->
      Lam varName body
    | -- varName conflicts
      isFree varName term ->
      let newName = freshName "x" term
          newBody = alphaConvert varName (Var newName) body
       in Lam newName (convert newBody)
    | otherwise ->
      Lam varName (convert body)
  where
    convert = alphaConvert name term
    isFree n e = n `elem` freeVars e
    freeVars = \case
      Var n -> [n]
      App f x -> freeVars f <> freeVars x
      Lam n b -> filter (/= n) (freeVars b)
    freshName s e
      | isFree s e = freshName (Text.cons 'x' s) e
      | otherwise = s

-- | Substitute lambda application
-- >>> betaReduce (App (Lam "x" (Var "x")) (Var "y"))
-- Var "y"
betaReduce :: Term -> Term
betaReduce = \case
  App t1 t2 -> case betaReduce t1 of
    Lam name body -> betaReduce (alphaConvert name t2 body)
    term -> App term t2
  term -> term

-- | The Value data type
data Value
  = VLam Name Term
  | VFun (Value -> Value)
  | VLit Literal
  deriving stock (Eq, Show)

data Literal
  = LitInt Integer
  deriving stock (Eq, Show)

instance Show (Value -> Value) where
  show = const "runtime-func"

instance Eq (Value -> Value) where
  _ == _ = True

-- | Evaluate term to value
eval :: Map Name Value -> Term -> Either Text Value
eval env = \case
  Var x -> case lookup x env of
    Just v -> pure v
    Nothing -> VLit <$> maybeToRight "invalid literal" (decodeLit x)
  Lam name expr -> pure $ VLam name expr
  App t1 t2 ->
    case eval env t1 of
      Right (VLam name body) -> eval env (alphaConvert name t2 body)
      Right (VFun f) -> f <$> eval env t2
      _ -> Left ("not a func: " <> show t1)
  where
    decodeLit :: Name -> Maybe Literal
    decodeLit n = LitInt <$> readMaybe (toString n)
