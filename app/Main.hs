module Main where

import Data.Foldable as F
import Data.Traversable as T
import System.Directory as D

data Fileinfo = Fileinfo {
  _filename :: FilePath, -- aka String
  _size :: Integer,
  _isdir :: Bool
}

main :: IO ()
main = do
  files <- D.listDirectory "."
  files' <- elaborateFiles files
  F.for_ files' putFileinfoLn

elaborateFiles :: [FilePath] -> IO [Fileinfo]
elaborateFiles files = do
  T.for files $ \file -> 
    Fileinfo file <$> D.getFileSize file <*> D.doesDirectoryExist file

putFileinfoLn :: Fileinfo -> IO ()
putFileinfoLn = putStrLn . renderFileinfo

renderFileinfo :: Fileinfo -> String
renderFileinfo file | _isdir file = _filename file ++ ", directory"
renderFileinfo file | _size file >= 1024 = _filename file ++ ", " ++ show (_size file `div` 1024) ++ " kilobyte(s)"
renderFileinfo file = _filename file ++ ", " ++ show (_size file) ++ " byte(s)"
