module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-XOverloadedStrings"
  , "-isrc"
  , "src/Main.hs"
  ]
