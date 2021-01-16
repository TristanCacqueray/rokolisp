module Main (main) where

import Relude
import RokoLisp
import Test.DocTest
import Test.Hspec
import Test.QuickCheck

newtype ATerm = ATerm {unTerm :: Term}
  deriving stock (Show, Eq)

instance Arbitrary ATerm where
  arbitrary = ATerm <$> frequency [(5, arbitraryVar), (2, arbitraryApp), (2, arbitraryLam)]
    where
      arbitraryText = toText . flip (:) [] <$> elements ['a' .. 'z']
      arbitraryLam = Lam <$> arbitraryText <*> (unTerm <$> arbitrary)
      arbitraryApp = App <$> (unTerm <$> arbitrary) <*> (unTerm <$> arbitrary)
      arbitraryVar = Var <$> arbitraryText

prop_parser :: ATerm -> Bool
prop_parser (ATerm t) = t == (parse' . format) t
  where
    parse' s = case parse s of
      Left err -> error err
      Right x -> x

docspec :: IO ()
docspec = doctest (opts <> ["-isrc", "src/"])
  where
    opts :: [String]
    opts =
      map
        (mappend "-X")
        [ "BangPatterns",
          "BinaryLiterals",
          "DataKinds",
          "DeriveGeneric",
          "DerivingStrategies",
          "ExplicitForAll",
          "FlexibleInstances",
          "GeneralizedNewtypeDeriving",
          "HexFloatLiterals",
          "ImportQualifiedPost",
          "LambdaCase",
          "MultiWayIf",
          "NamedFieldPuns",
          "NamedWildCards",
          "NoImplicitPrelude",
          "NumDecimals",
          "NumericUnderscores",
          "OverloadedLists",
          "OverloadedStrings",
          "PostfixOperators",
          "RecordWildCards",
          "ScopedTypeVariables",
          "StandaloneDeriving",
          "TupleSections",
          "TypeOperators"
        ]

main :: IO ()
main = hspec $ do
  describe "Doctest" $ it "pass" docspec
  describe "Syntax Properties" $ it "prop_parser" $ property prop_parser
