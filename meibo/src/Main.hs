module Main where

import Control.Applicative ((<$>))
import System.IO (stdout, hFlush)

import Command
import CommandParser
import Types

main :: IO ()
main = repl

repl :: IO ()
repl = do
    printPrompt
    readCommand >>= evalCommand >>= printResult
    repl

printPrompt :: IO ()
printPrompt = do
    putStr "> "
    hFlush stdout

readCommand :: IO (Either String Command)
readCommand = parseCommand <$> getLine

evalCommand :: Either String Command -> IO Result
evalCommand (Right cmd) = command cmd
evalCommand (Left  err) = return $ NG err

printResult :: Result -> IO ()
printResult OK       = putStrLn "OK" >> hFlush stdout
printResult (NG err) = putStrLn ("Error: " ++ err)  >> hFlush stdout
