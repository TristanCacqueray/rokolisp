-- | The language syntax
module RokoLisp.Syntax (parse, format) where

import Relude hiding ((<|>))
import RokoLisp.Eval (Term (..))
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
  Atom x -> pure $ Var x
  List [x] -> desugar x
  List (Atom "λ" : xs) -> desugar_lambda xs
  List (Atom "let" : xs) -> desugar_let xs
  List (f : x : xs) -> desugar_app (App <$> desugar f <*> desugar x) xs
  List [] -> Left "empty list"
  where
    desugar_lambda [x] = desugar x
    desugar_lambda (Atom x : xs) = Lam x <$> desugar_lambda xs
    desugar_lambda xs = Left ("Invalid lambda definition: " <> show xs)
    desugar_let :: [Syntax] -> Either Text Term
    desugar_let [Atom name, value, body] = App <$> (Lam name <$> desugar body) <*> desugar value
    desugar_let xs = Left ("Invalid let binding: " <> show xs)
    desugar_app :: Either Text Term -> [Syntax] -> Either Text Term
    desugar_app acc = \case
      [] -> acc
      (x : xs) -> desugar_app (App <$> acc <*> desugar x) xs

runSyntaxParser :: Text -> Either Text Syntax
runSyntaxParser s = case Parsec.runParser (syntaxParser <* Parsec.eof) () "<input>" s of
  Left err -> Left (show err)
  Right syntax -> Right syntax

syntaxParser :: Parser Syntax
syntaxParser = (Atom <$> atomParser) <|> (List <$> listParser)
  where
    atomParser =
      toText
        <$> (spaces *> Parsec.many1 (Parsec.satisfy (not . flip elem ("()\n " :: String))) <* spaces)
    listParser =
      (spaces *> char '(') *> syntaxParser `Parsec.sepBy` spaces <* (spaces *> char ')' <* spaces)
