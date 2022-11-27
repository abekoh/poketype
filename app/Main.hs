module Main (main) where

import Lib
import Options.Applicative
import Control.Monad
import Data.Semigroup ()

showChart :: IO ()
showChart = putStrLn generateTypeChart

opts :: Parser (IO ())
opts = subparser ( command "chart" (info (pure showChart) idm) )

main :: IO ()
main = join $ execParser (info opts idm)

