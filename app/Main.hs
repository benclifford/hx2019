module Main where

import System.Directory as D

main :: IO ()
main = do
  putStrLn "myls"
  files <- D.listDirectory "."
  print files
