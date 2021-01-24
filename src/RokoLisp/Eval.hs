-- | The language evaluator
module RokoLisp.Eval
  ( -- * Eval data types
    Term (..),
    Value (..),
    Literal (..),

    -- * Runtimes
    Name,
    ThunkRef,
    toVLam,

    -- * Evaluation function
    eval,
  )
where

import Data.Map (lookup)
import qualified Data.Map as Map
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

-- | The Value data type
data Value
  = VLam Name Term (Thunk -> IO Value)
  | VFun (Value -> IO Value)
  | VLit Literal
  | VIO (IO ())

data Literal
  = LitInt Integer
  | LitText Text
  deriving stock (Eq)

instance Show Value where
  show (VLit (LitInt x)) = show x
  show (VLit (LitText x)) = toString x
  show (VLam name term _) = show (Lam name term)
  show (VFun _) = "<runtime-func>"
  show (VIO _) = "<io-value>"

instance Eq Value where
  VLam n t _ == VLam n' t' _ = n == n' && t == t'
  VLit x == VLit y = x == y
  _ == _ = False

-- | call-by-value thunk (based on Write You A Haskell code by Stephen Dielh)
type Thunk = () -> IO Value

type ThunkRef = IORef Thunk

type Env = Map Name ThunkRef

toVLam :: Env -> Name -> Term -> Value
toVLam env name term = VLam name term (mkThunk env name term)

forceThunk :: MonadIO m => ThunkRef -> m Value
forceThunk ref = do
  th <- readIORef ref
  v <- liftIO $ th ()
  writeIORef ref (\() -> return v)
  return v

mkThunk :: MonadIO m => Env -> Name -> Term -> (Thunk -> m Value)
mkThunk env x body th = do
  thRef <- newIORef th
  eval (Map.insert x thRef env) body

-- | Evaluate term to value
eval :: MonadIO m => Env -> Term -> m Value
eval env = \case
  Var x -> case lookup x env of
    Just th -> forceThunk th
    Nothing -> case decodeLit x of
      Just v -> pure $ VLit v
      Nothing -> error ("unknown var " <> show x)
  Lam name term -> pure $ toVLam env name term
  App t1 t2 -> do
    fun <- eval env t1
    case fun of
      VLam _ _ c -> liftIO $ c (const $ eval env t2)
      VFun f -> do
        arg <- eval env t2
        liftIO $ f arg
      x -> error ("app term is not a closure " <> show t1 <> " (" <> show x <> ")")
  where
    decodeLit :: Name -> Maybe Literal
    decodeLit n
      | Text.head n == '"' && Text.last n == '"' = LitText <$> readMaybe (toString n)
      | otherwise = LitInt <$> readMaybe (toString n)
