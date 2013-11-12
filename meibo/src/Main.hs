module Main where

import Control.Applicative ((<$>))
import System.IO (stdout, hFlush)
import Data.IORef

import Command
import CommandParser
import Types

main :: IO ()
main = newIORef [] >>= repl

repl :: IORef [Person] -> IO ()
repl ref = do
    printPrompt
    readCommand >>= evalCommand ref >>= printResult
    repl ref

printPrompt :: IO ()
printPrompt = do
    putStr "> "
    hFlush stdout

readCommand :: IO (Either String Command)
readCommand = parseCommand <$> getLine

evalCommand :: IORef [Person] -> Either String Command -> IO Result
evalCommand ref (Right cmd) = command ref cmd
evalCommand _   (Left  err) = return $ NG err

printResult :: Result -> IO ()
printResult OK       = putStrLn "OK" >> hFlush stdout
printResult (NG err) = putStrLn ("Error: " ++ err)  >> hFlush stdout
