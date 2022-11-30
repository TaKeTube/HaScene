module Main where

import           Options.Applicative

import           UI.Game             (playGame)

data Opts = Opts
  { fps      :: Maybe Int
  , initFile :: Maybe String
  }

opts :: Parser Opts
opts = Opts
  <$> optional (option auto
    (  long "fps"
    <> short 'f'
    <> metavar "FPS"
    <> help "FPS setting" ))
  <*> optional (option auto
    (  long "init-file"
    <> short 'i'
    <> metavar "INITFILE"
    <> help "Specify a init file" ))

fullopts :: ParserInfo Opts
fullopts = info (helper <*> opts)
  (  fullDesc
  <> header "HaScene - ASCII 3D render." )

main :: IO ()
main = do
  (Opts f filename) <- execParser fullopts           -- get CLI opts/args
  _ <- playGame f filename                                -- play game
  return ()

printM :: Show a => Maybe a -> IO ()
printM Nothing  = putStrLn "None"
printM (Just s) = print s
