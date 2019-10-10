{-# Language ViewPatterns #-}
{-# options_ghc -fwarn-incomplete-patterns -Werror #-}
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
  getFiles >>= elaborateFiles >>= filterFiles >>= sortFiles >>= printFiles

filterFiles :: [Fileinfo] -> MyApp [Fileinfo]
filterFiles files = do
  h <- _hidden <$> ask
  if h then pure files
       else pure (filter check files)
  where check file = (head . _filename) file /= '.'

sortFiles :: [Fileinfo] -> MyApp [Fileinfo]
sortFiles f = do
  c <- getSorter
  pure (L.sortBy c f)

  -- or in applicative style, pushing the pure deeper down:
  -- L.sortBy <$> getSorter <*> (pure f)

  where 
    getSorter :: MyApp (Fileinfo -> Fileinfo -> OR.Ordering)
    getSorter = do
      mode <- _sortMode <$> ask
      pure $ case mode of 
        Alpha -> OR.comparing _filename
        Size -> OR.comparing _size
        NameLength -> OR.comparing (length . _filename)
        None -> \l r -> OR.EQ

-- (OR.comparing _filename)
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
  _long :: Bool,
  _sortMode :: SortMode,
  _hidden :: Bool
}

data SortMode = None | Alpha | Size | NameLength deriving Read

cliParser :: O.Parser MylsOptions
cliParser =
  MylsOptions <$> O.strArgument (O.metavar "PATH" <> O.value ".")
              <*> O.switch (long "long" <> short 'l' <> help "Enable long output")
              <*> O.option O.auto (long "sort" <> O.metavar "MODE" <> help "sort mode" <> O.value Alpha)
              <*> O.switch (long "hidden" <> help "Show hidden files")

cliParserInfo :: O.ParserInfo MylsOptions
cliParserInfo = O.info (O.helper <*> cliParser) (mempty)
