module Main where

import Control.Applicative ((<$>))
import System.IO (stdout, hFlush)

import Command
import CommandParser

main :: IO ()
main = repl

repl :: IO ()
repl = do
    printPrompt
    ecmd <- parseCommand <$> getLine
    case ecmd of
        Right cmd -> command cmd
        Left  err -> printError err
    repl

printPrompt :: IO ()
printPrompt = do
    putStr "> "
    hFlush stdout

printError :: String -> IO ()
printError err = do
    putStr err
    hFlush stdout
