module Main (main) where

import Lib

main :: IO ()
main = do
  putStrLn generateTypeChart
  putStrLn $ generateWeaknessList (Pokemon Fairy Ghost)
  putStrLn $ generateWeaknessList (Pokemon Grass Dark)
  putStrLn $ generateWeaknessList (Pokemon Poison Ground)
