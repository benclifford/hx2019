{-# Language ViewPatterns #-}
module Main where

import Data.Foldable as F
import Data.Traversable as T
import Options.Applicative as O
import System.Console.ANSI as A
import System.Directory as D

data Fileinfo = Fileinfo {
  _filename :: FilePath, -- aka String
  _size :: Integer,
  _isdir :: Bool
}

main :: IO ()
main = do

  opts <- O.execParser cliParserInfo

  D.setCurrentDirectory (_path opts)
  files <- D.listDirectory "."

  files' <- elaborateFiles files

  let printer = case _long opts of
        True -> putFileinfoLn
        False -> (putStrLn . _filename)
  F.for_ files' printer

elaborateFiles :: [FilePath] -> IO [Fileinfo]
elaborateFiles files = do
  T.for files $ \file -> 
    Fileinfo file <$> D.getFileSize file <*> D.doesDirectoryExist file

putFileinfoLn :: Fileinfo -> IO ()
putFileinfoLn f = do
  putStr (_filename f)
  putStr ", "
  if _isdir f
    then do
      setColor A.Green
      putStr "directory"
      setNormal
    else do
      setColor A.Cyan
      (putStr . prettySize . _size) f
      -- or putStr (prettySize (_size f))
      setNormal
  putStrLn ""

prettySize :: Integer -> String
prettySize (prefixify -> (s, p)) | s == 1 = show s ++ " " ++ p ++ "byte"
prettySize (prefixify -> (s, p)) = show s ++ " " ++ p ++ "bytes"

prefixify :: Integer -> (Integer, String)
prefixify s | s >= 1024 = (s `div` 1024, "kilo")
prefixify s = (s, "")


setColor :: A.Color -> IO ()
setColor col = A.setSGR [A.SetColor A.Foreground A.Vivid col]

setNormal :: IO ()
setNormal = A.setSGR []



data MylsOptions = MylsOptions {
  _path :: String,
  _long :: Bool
}

cliParser :: O.Parser MylsOptions
cliParser =
  MylsOptions <$> O.strArgument (O.metavar "PATH" <> O.value ".")
              <*> O.switch (long "long" <> short 'l' <> help "Enable long output")

cliParserInfo :: O.ParserInfo MylsOptions
cliParserInfo = O.info (O.helper <*> cliParser) (mempty)
