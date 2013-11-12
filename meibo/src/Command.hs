module Command (Command(..), command) where

import Control.Applicative ((<$>))
import Control.Exception
import qualified Control.Exception as E
import Data.IORef
import System.Exit (exitSuccess)

import CSVParser
import Person
import Types

command :: IORef [Person] -> Command -> IO Result
command ref cmd = eval ref cmd `E.catch` \(SomeException e) ->
    return $ NG (show e)

eval :: IORef [Person] -> Command -> IO Result
eval _    Quit       = exitSuccess
eval ref (Read file) = comRead ref file
eval ref Check       = comCheck ref
eval ref (Print n)   = comPrint ref n
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

-- FIXME: 0 and minus
comPrint :: IORef [Person] -> Int -> IO Result
comPrint ref n = do
    db <- readIORef ref
    mapM print $ take n db
    return OK
