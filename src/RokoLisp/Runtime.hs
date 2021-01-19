-- | The language runtime
module RokoLisp.Runtime (functions) where

import Relude
import RokoLisp.Eval

add :: Value -> Value -> IO Value
add x y = case (x, y) of
  (VLit (LitInt x'), VLit (LitInt y')) -> pure $ VLit (LitInt (x' + y'))
  _ -> error "Invalid add arguments"

inc :: Value -> IO Value
inc = add (VLit (LitInt 1))

churchNumeralDecode :: Value -> IO Value
churchNumeralDecode x = case x of
  VClosure c -> do
    y <- c (const $ pure (VFun inc))
    case y of
      VClosure f -> f (const $ pure (VLit (LitInt 0)))
      _ -> error ("Invalid church numeral: second term is not a lambda: " <> show y)
  _ -> error ("Invalid church numeral: first term is not a lambda: " <> show x)

churchNumeralEncode :: Value -> IO Value
churchNumeralEncode = \case
  VLit (LitInt x) | x >= 0 -> pure $ VLam "f" (Lam "s" (go x))
  x -> error ("Church numeral encode failed: " <> show x <> " is not a literal natural")
  where
    go 0 = Var "s"
    go n = App (Var "f") (go (n - 1))

functions :: MonadIO m => m (Map Name ThunkRef)
functions = sequence (mkRuntimeThunk <$> runtimeMap)
  where
    runtimeMap :: Map Name (Value -> IO Value)
    runtimeMap =
      fromList
        [ ("church_numeral_decode", churchNumeralDecode),
          ("church_numeral_encode", churchNumeralEncode)
        ]
    mkRuntimeThunk :: MonadIO m => (Value -> IO Value) -> m ThunkRef
    mkRuntimeThunk v = newIORef (const $ pure $ VFun v)
