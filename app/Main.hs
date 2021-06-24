module Main where

import Core.ArithC (ArithC (..), interpArithC)

main :: IO ()
main = do
  let astExample = Value 5
  print . interpArithC $ astExample
