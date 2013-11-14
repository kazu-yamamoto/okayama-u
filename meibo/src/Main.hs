module Main where

import Control.Applicative ((<$>))
import System.IO (stdout, hFlush)
import Data.IORef

import Command
import CommandParser
import Types

main :: IO ()
main = newIORef defaultDB >>= repl

repl :: IORef DB -> IO ()
repl ref = do
    printPrompt
    readCommand >>= evalCommand ref >>= printResult
    repl ref

printPrompt :: IO ()
printPrompt = putStr "> " >> hFlush stdout

readCommand :: IO (Either ParserError Command)
readCommand = parseCommand <$> getLine

evalCommand :: IORef DB
            -> Either ParserError Command
            -> IO Result
evalCommand ref (Right cmd) = command ref cmd
evalCommand _   (Left  err) = return $ NG err

printResult :: Result -> IO ()
printResult (OK ms)  = do
    mapM_ putStrLn ms
    putStrLn "OK"
printResult (NG err) = do
    putStrLn $ "Error: " ++ err
    putStrLn "NG"
