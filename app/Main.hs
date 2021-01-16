module Main (main) where

import Relude
import RokoLisp (parse)

main :: IO ()
main = print (parse "(λ x x)")
