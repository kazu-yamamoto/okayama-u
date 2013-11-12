module Command (Command(..), command) where

import Control.Applicative ((<$>))
import Control.Exception
import Data.IORef
import System.Exit (exitSuccess)

import CSVParser
import Person
import Types

command :: IORef [Person] -> Command -> IO Result
command ref cmd = eval ref cmd `catches` []

eval :: IORef [Person] -> Command -> IO Result
eval _    Quit       = exitSuccess
eval ref (Read file) = comRead ref file
eval ref Check       = comCheck ref
eval _   _           = return $ NG "not implemented"

comRead :: IORef [Person] -> FilePath -> IO Result
comRead ref file = do
    csv <- readFile file
    case parseCSV csv of
        Left err -> return $ NG err
        Right es -> do
            writeIORef ref $ map fromCSV es
            return OK

comCheck :: IORef [Person] -> IO Result
comCheck ref = do
    len <- length <$> readIORef ref
    putStrLn $ show len ++ " entries in DB"
    return OK
