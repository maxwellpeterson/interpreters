module Main where

import Core.ArithC (ArithC (..), interp)

main :: IO ()
main = do
  let astExample = Value 5
  print . interp $ astExample
