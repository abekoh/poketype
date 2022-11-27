module Main (main) where

import Lib
import Options.Applicative
import Data.Semigroup ((<>))

data Cli = Cli { hello :: String }

cli :: Parser Cli
cli = Cli
  <$> strOption
     ( long "hello"
    <> metavar "TARGET"
    <> help "Target for the greeting" )

-- sample :: Parser Sample
-- sample = Sample
--       <$> strOption
--           ( long "hello"
--          <> metavar "TARGET"
--          <> help "Target for the greeting" )
--       <*> switch
--           ( long "quiet"
--          <> short 'q'
--          <> help "Whether to be quiet" )
--       <*> option auto
--           ( long "enthusiasm"
--          <> help "How enthusiastically to greet"
--          <> showDefault
--          <> value 1
--          <> metavar "INT" )
-- main :: IO ()
-- main = do
  -- putStrLn generateTypeChart
  -- putStrLn $ generateWeaknessList (Pokemon Fairy Ghost)
  -- putStrLn $ generateWeaknessList (Pokemon Grass Dark)
  -- putStrLn $ generateWeaknessList (Pokemon Poison Ground)
  --
  --
main :: IO ()
main = handler =<< execParser opts
  where
    opts = info (cli <**> helper)
      ( fullDesc
     <> progDesc "Print pokemon type information"
     <> header "poketype - a cli for checking Pokemon types" )

-- greet :: Sample -> IO ()
-- greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
-- greet _ = return ()

handler :: Cli -> IO ()
handler (Cli h) = putStrLn $ "Hello " ++ h
