-- | The language syntax
module RokoLisp.Syntax (parse, format, resolve) where

import qualified Data.Text as Text
import Relude hiding ((<|>))
import RokoLisp.Eval (Term (..))
import System.FilePath (takeDirectory, (</>))
import Text.Parsec
  ( char,
    spaces,
    (<|>),
  )
import qualified Text.Parsec as Parsec

type Parser a = Parsec.ParsecT Text () Identity a

data Syntax
  = Atom Text
  | List [Syntax]
  deriving stock (Show, Eq)

-- | Parse a term
--
-- # Basic terms:
-- >>> parse " x "
-- Right (Var "x")
-- >>> parse "(λ x x)"
-- Right (Lam "x" (Var "x"))
-- >>> parse "(f x)"
-- Right (App (Var "f") (Var "x"))
--
-- # Application priority:
-- >>> parse "(a b c)"
-- Right (App (App (Var "a") (Var "b")) (Var "c"))
-- >>> parse "(a (b c) d)"
-- Right (App (App (Var "a") (App (Var "b") (Var "c"))) (Var "d"))
parse :: Text -> Either Text Term
parse s = runSyntaxParser s >>= desugar

format :: Term -> Text
format = \case
  Var x -> x
  Lam name body -> "(λ " <> name <> " " <> format body <> ")"
  App f x -> "(" <> format f <> " " <> format x <> ")"

-- | Remove syntax desugar
--
-- # Lambda curry
-- >>> parse "(λ x y x)"
-- Right (Lam "x" (Lam "y" (Var "x")))
--
-- # Application curry
-- >>> parse "(add x y)"
-- Right (App (App (Var "add") (Var "x")) (Var "y"))
desugar :: Syntax -> Either Text Term
desugar = \case
  Atom "true" -> pure $ Lam "x" (Lam "y" (Var "x"))
  Atom "false" -> pure $ Lam "x" (Lam "y" (Var "y"))
  Atom "nil" -> Lam "_" <$> desugar (Atom "true")
  Atom x -> pure $ Var x
  List [x] -> desugar x
  List (Atom x : xs)
    | Text.head x == 'λ' && Text.tail x /= "" -> desugar_lambda (Atom (Text.tail x) : xs)
  List (Atom "λ" : xs) -> desugar_lambda xs
  List (Atom "let" : xs) -> desugar_let xs
  List (Atom "cons" : xs) -> desugar_pair xs
  List (Atom "list" : xs) -> desugar_list xs
  List (Atom "do" : xs) -> desugar_do xs
  List (f : x : xs) -> desugar_app (App <$> desugar f <*> desugar x) xs
  List [] -> Left "empty list"
  where
    desugar_lambda [x] = desugar x
    desugar_lambda (Atom x : xs) = Lam x <$> desugar_lambda xs
    desugar_lambda xs = Left ("Invalid lambda definition: " <> show xs)
    desugar_let :: [Syntax] -> Either Text Term
    desugar_let [Atom name, value, Atom "in", body] = desugar_let [Atom name, value, body]
    desugar_let [Atom name, value, body] = App <$> (Lam name <$> desugar body) <*> desugar value
    desugar_let (Atom name : value : xs) = App <$> (Lam name <$> desugar_let xs) <*> desugar value
    desugar_let xs = Left ("Invalid let binding: " <> show xs)
    desugar_pair :: [Syntax] -> Either Text Term
    desugar_pair [a, b] = do
      a' <- desugar a
      b' <- desugar b
      pure $ Lam "s" (App (App (Var "s") a') b')
    desugar_pair x = Left ("Invalid pair: " <> show x)
    desugar_list [] = desugar (Atom "nil")
    desugar_list (x : xs) = cons x =<< desugar_list xs
      where
        cons a b = do
          a' <- desugar a
          pure $ Lam "s" (App (App (Var "s") a') b)
    desugar_do :: [Syntax] -> Either Text Term
    desugar_do [x] = desugar x
    desugar_do (x : xs) = do
      x' <- desugar x
      xs' <- desugar_do xs
      pure $ App (App (Var ">>") x') xs'
    desugar_do [] = Left "Empty do block"
    desugar_app :: Either Text Term -> [Syntax] -> Either Text Term
    desugar_app acc = \case
      [] -> acc
      (x : xs) -> desugar_app (App <$> acc <*> desugar x) xs

runSyntaxParser :: Text -> Either Text Syntax
runSyntaxParser s = case Parsec.runParser (syntaxParser <* Parsec.eof) () "<input>" s of
  Left err -> Left (show err)
  Right syntax -> Right syntax

syntaxParser :: Parser Syntax
syntaxParser = comments *> ((Atom <$> atomParser) <|> (List <$> listParser)) <* comments
  where
    comments =
      Parsec.optional
        ( char ';' >> Parsec.many1 (Parsec.satisfy ('\n' /=)) <* spaces
        )
    atomParser =
      toText
        <$> (spaces *> Parsec.many1 (Parsec.satisfy (not . flip elem ("()\n " :: String))) <* spaces)
    listParser =
      (spaces *> char '(') *> syntaxParser `Parsec.sepBy` spaces <* (spaces *> char ')' <* spaces)

-- | Resolve imports
resolve :: MonadIO m => FilePath -> Term -> m Term
resolve fp = \case
  Lam name body -> Lam name <$> resolve fp body
  App f arg -> App <$> resolve fp f <*> resolve fp arg
  Var x -> importVar x
  where
    importVar :: MonadIO m => Text -> m Term
    importVar x
      | Text.head x `elem` ("./" :: String) = do
        let newFp = takeDirectory fp </> toString x
        terms' <- parse <$> readFileText newFp
        case terms' of
          Right term -> resolve newFp term
          Left err -> error ("Import " <> x <> " failed: " <> err)
      | otherwise = pure (Var x)
