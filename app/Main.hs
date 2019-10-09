{-# Language ViewPatterns #-}
module Main where

import Data.Foldable as F
import Data.Traversable as T
import Options.Applicative as O
import System.Directory as D

data Fileinfo = Fileinfo {
  _filename :: FilePath, -- aka String
  _size :: Integer,
  _isdir :: Bool
}

main :: IO ()
main = do

  opts <- O.execParser cliParserInfo

  D.setCurrentDirectory opts

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
renderFileinfo file = _filename file ++ ", " ++ prettySize (_size file)

prettySize :: Integer -> String
prettySize (prefixify -> (s, p)) | s == 1 = show s ++ " " ++ p ++ "byte"
prettySize (prefixify -> (s, p)) = show s ++ " " ++ p ++ "bytes"

prefixify :: Integer -> (Integer, String)
prefixify s | s >= 1024 = (s `div` 1024, "kilo")
prefixify s = (s, "")


cliParser :: O.Parser String
cliParser = O.strArgument (O.metavar "PATH" <> O.value ".")

cliParserInfo :: O.ParserInfo String
cliParserInfo = O.info (O.helper <*> cliParser) (mempty)
