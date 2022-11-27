module Main (main) where

import Lib
import Options.Applicative
import Control.Monad
import Data.Semigroup ()

showChart :: IO ()
showChart = putStr generateTypeChart

showWeakness :: String -> IO()
showWeakness = putStr . generateWeaknessList

opts :: Parser (IO ())
opts = subparser ( command "chart" (info (pure showChart) idm)
  <> command "weakness" (info (showWeakness <$> argument str idm) idm)
  )

main :: IO ()
main = join $ execParser (info opts idm)

