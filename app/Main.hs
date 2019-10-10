{-# Language ViewPatterns #-}
module Main where

import Control.Monad.Reader as R
import Control.Monad.IO.Class (liftIO)

import Data.List as L
import Data.Foldable as F
import Data.Ord as OR
import Data.Traversable as T
import Options.Applicative as O
import System.Console.ANSI as A
import System.Directory as D

data Fileinfo = Fileinfo {
  _filename :: FilePath, -- aka String
  _size :: Integer,
  _isdir :: Bool
}

type MyApp = ReaderT MylsOptions IO

main :: IO ()
main = do

  opts <- O.execParser cliParserInfo

  R.runReaderT rest opts

rest :: MyApp ()
rest = do
  getFiles >>= elaborateFiles >>= sortFiles >>= printFiles


sortFiles :: [Fileinfo] -> MyApp [Fileinfo]
sortFiles = pure . L.sortBy (OR.comparing _filename)
-- or:
-- sortFiles = pure . L.sortBy (OR.comparing _size)
-- or by a function that *isn't* a field accessor:
-- sortFiles = pure . L.sortBy (OR.comparing (length . _filename))



getFiles :: MyApp [FilePath]
getFiles = do
  opts <- ask
  liftIO $ do
    D.setCurrentDirectory (_path opts)
    D.listDirectory "."

printFiles :: [Fileinfo] -> MyApp ()
printFiles files = do
  -- this becomes an action rather than a pure case
  -- so the let turns into <-
  -- because we're doing an action now -- reading options
  printer <- do
    opts <- ask
    pure $ case _long opts of
             True -> putFileinfoLn
             False -> (liftIO . putStrLn . _filename)
  F.for_ files printer




elaborateFiles :: [FilePath] -> MyApp [Fileinfo]
elaborateFiles files = do
  T.for files $ \file -> 
    liftIO $ Fileinfo file <$> D.getFileSize file <*> D.doesDirectoryExist file

putFileinfoLn :: Fileinfo -> MyApp ()
putFileinfoLn f = liftIO $ do
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
