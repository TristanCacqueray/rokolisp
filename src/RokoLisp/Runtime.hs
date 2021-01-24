-- | The language runtime
module RokoLisp.Runtime (functions, consDecode, listDecode) where

import Relude
import RokoLisp.Eval

add :: Value -> Value -> IO Value
add x y = case (x, y) of
  (VLit (LitInt x'), VLit (LitInt y')) -> pure $ VLit (LitInt (x' + y'))
  _ -> error "Invalid add arguments"

inc :: Value -> IO Value
inc = add (VLit (LitInt 1))

-- | Decode a church encoded cons value
consDecode :: Value -> IO (Value, Value)
consDecode x = case x of
  VLam _ _ c -> do
    car <- c (const $ pure true)
    cdr <- c (const $ pure false)
    pure (car, cdr)
  _ -> error ("Invalid cons: first term is not a lambda: " <> show x)

-- | Decode a church encoded list
listDecode :: Value -> IO [Value]
listDecode x = do
  (car, cdr) <- consDecode x
  case cdr of
    VLam _ _ f -> do
      isNil <- isTrue =<< f (const $ pure isNull)
      case isNil of
        Just True -> pure [car]
        _ -> fmap (car :) (listDecode cdr)
    _ -> error ("Invalid list: second term is not a lambda: " <> show cdr)

-- | Test if a church encoded boolean is True
isTrue :: Value -> IO (Maybe Bool)
isTrue (VLam _name _term f) = do
  x <- f (const $ pure $ VLit (LitInt 0))
  case x of
    VLam _ _ g -> do
      y <- g (const $ pure $ VLit (LitInt 1))
      pure $ case y of
        VLit (LitInt z) -> Just $ z == 0
        _ -> Nothing
    _ -> pure Nothing
isTrue _ = pure Nothing

isNull :: Value
isNull = toVLam mempty "x" (Lam "y" (Var "x"))

churchNumeralDecode :: Value -> IO Value
churchNumeralDecode x = case x of
  VLam _ _ c -> do
    y <- c (const $ pure (VFun inc))
    case y of
      VLam _ _ f -> f (const $ pure (VLit (LitInt 0)))
      _ -> error ("Invalid church numeral: second term is not a lambda: " <> show y)
  _ -> error ("Invalid church numeral: first term is not a lambda: " <> show x)

churchNumeralEncode :: Value -> IO Value
churchNumeralEncode = \case
  VLit (LitInt x) | x >= 0 -> pure $ toVLam mempty "f" (Lam "s" (go x))
  x -> error ("Church numeral encode failed: " <> show x <> " is not a literal natural")
  where
    go 0 = Var "s"
    go n = App (Var "f") (go (n - 1))

true :: Value
true = toVLam mempty "x" (Lam "y" (Var "x"))

false :: Value
false = toVLam mempty "x" (Lam "y" (Var "y"))

equals :: Value -> Value -> IO Value
equals (VLit x) (VLit y) = pure $ if x == y then true else false
equals _ _ = error "Invalid argument for equals"

fn2 :: (Value -> Value -> IO Value) -> Value -> IO Value
fn2 f x = pure $ VFun (f x)

print' :: Value -> IO Value
print' x = pure $ VIO (putText (show x))

seqIO :: Value -> Value -> IO Value
seqIO x y = case (x, y) of
  (VIO x', VIO y') -> pure $ VIO (x' >> y')
  _ -> error "Can't sequence non IO value"

functions :: MonadIO m => m (Map Name ThunkRef)
functions = sequence (mkRuntimeThunk <$> runtimeMap)
  where
    runtimeMap :: Map Name (Value -> IO Value)
    runtimeMap =
      fromList
        [ ("church_numeral_decode", churchNumeralDecode),
          ("church_numeral_encode", churchNumeralEncode),
          ("equals?", fn2 equals),
          ("print", print'),
          (">>", fn2 seqIO)
        ]
    mkRuntimeThunk :: MonadIO m => (Value -> IO Value) -> m ThunkRef
    mkRuntimeThunk v = newIORef (const $ pure $ VFun v)
