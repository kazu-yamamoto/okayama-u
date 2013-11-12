module Command (Command(..), command) where

import Control.Applicative ((<$>))
import Control.Exception
import qualified Control.Exception as E
import Data.Function (on)
import Data.IORef
import Data.List (sortBy, isInfixOf)
import System.Exit (exitSuccess)

import CSVParser
import Person
import Types
import Utils

command :: IORef [Person] -> Command -> IO Result
command ref cmd = eval ref cmd `E.catch` \(SomeException e) ->
    return $ NG (show e)

eval :: IORef [Person] -> Command -> IO Result
eval _    Quit       = exitSuccess
eval ref (Read file) = comRead ref file
eval ref Check       = comCheck ref
eval ref (Print n)   = comPrint ref n
eval ref (Sort n)    = comSort ref n
eval ref (Find word) = comFind ref word
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

comPrint :: IORef [Person] -> Int -> IO Result
comPrint ref n = do
    db <- readIORef ref
    let db' | n == 0    = db
            | n >  0    = take n db
            | otherwise = takeEnd (negate n) db
    mapM_ print db'
    return OK

comSort :: IORef [Person] -> Int -> IO Result
comSort ref n = do
    modifyIORef ref (sortBy (compareEntry n))
    return OK

-- FIXME: is N 0-origin?
compareEntry :: Int-> Person -> Person -> Ordering
compareEntry 0 = compare `on` personId
compareEntry 1 = compare `on` personName
compareEntry 2 = compare `on` personBirthday
compareEntry 3 = compare `on` personAddress
compareEntry 4 = compare `on` personMisc
compareEntry _ = error "no such item"

comFind :: IORef [Person] -> String -> IO Result
comFind ref word = do
    db <- readIORef ref
    let db' = filter predicate db
    mapM_ print db'
    return OK
  where
    predicate psn = word `isInfixOf` personName psn
                 || word `isInfixOf` personAddress psn
                 || word `isInfixOf` personMisc psn
