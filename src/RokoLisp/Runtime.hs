-- | The language runtime
module RokoLisp.Runtime (functions) where

import Relude
import RokoLisp.Eval

fn2 :: (Value -> Value -> Value) -> Value -> Value
fn2 f = VFun . f

add :: Value -> Value -> Value
add x y = case (x, y) of
  (VLit (LitInt x'), VLit (LitInt y')) -> VLit (LitInt (x' + y'))
  _ -> error "Invalid add arguments"

inc :: Value -> Value
inc = add (VLit (LitInt 1))

churchNumeralDecode :: Value -> Value
churchNumeralDecode x = case x of
  VLam f body -> case eval functions body of
    Right (VLam arg body') -> case eval functions (App (App (Lam f (Lam arg body')) (Var "+1")) (Var "0")) of
      Right v -> v
      Left e -> error ("Church numeral decode failed: " <> e)
    _ -> error ("Invalid church numeral: " <> show x)
  _ -> error ("Invalid church numeral: " <> show x)

churchNumeralEncode :: Value -> Value
churchNumeralEncode = \case
  VLit (LitInt x) | x >= 0 -> VLam "f" (Lam "s" (go x))
  x -> error ("Church numeral encode failed: " <> show x <> " is not a literal natural")
  where
    go 0 = Var "s"
    go n = App (Var "f") (go (n - 1))

functions :: Map Text Value
functions =
  VFun
    <$> fromList
      [ ("+1", inc),
        ("+", fn2 add),
        ("church_numeral_decode", churchNumeralDecode),
        ("church_numeral_encode", churchNumeralEncode)
      ]
