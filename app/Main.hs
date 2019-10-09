module Main where

import Data.Foldable as F
import System.Directory as D

main :: IO ()
main = do
  files <- D.listDirectory "."
  F.for_ files putStrLn
