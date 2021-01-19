module Main (main) where

import qualified Data.Text as Text
import Gauge.Main
import Relude
import RokoLisp
import System.Random

sampleTerm :: Int -> Term
sampleTerm = go (mkStdGen 42)
  where
    range :: (Int, Int)
    range = (1, 2)
    go :: RandomGen g => g -> Int -> Term
    go _ 0 = Var "x"
    go g n =
      let (n', (v, g')) = (n - 1, randomR range g)
          go' = go g' n'
       in case v of
            1 -> Lam "y" go'
            2 -> App go' go'
            _ -> go'

parseFormat :: Text -> Either Text Text
parseFormat s = format <$> parse s

main :: IO ()
main =
  defaultMain
    [ bench ("formatParse (" <> show (Text.length term) <> " byte)") (whnf parseFormat term),
      bench "fact 7" (whnfIO (evalFact 5040 "(./test/code/fact.rkl 7)"))
    ]
  where
    evalFact :: Integer -> Text -> IO ()
    evalFact expected t = do
      value <- doEval t
      case value of
        VLit (LitInt x) | x == expected -> pure ()
        err -> error ("Eval failed: " <> show err)

    term = format . sampleTerm $ 24
