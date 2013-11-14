module Command (Command(..), command) where

import Control.Applicative ((<$>))
import Control.Exception
import Data.Either (lefts, rights)
import Data.Function (on)
import Data.IORef
import Data.List (sortBy, isInfixOf, intercalate)
import System.Exit (exitSuccess, ExitCode)
import System.IO (withFile, hPutStrLn, IOMode(..))

import CSVParser
import Person
import Types
import Utils

command :: IORef [Person] -> Command -> IO Result
command ref cmd = eval ref cmd `catches` [Handler ehandler, Handler shandler]
  where
    ehandler :: ExitCode -> IO Result
    ehandler _ = exitSuccess
    shandler :: SomeException -> IO Result
    shandler e = return $ NG (show e)

eval :: IORef [Person] -> Command -> IO Result
eval _    Quit        = exitSuccess
eval ref (Read file)  = comRead ref file
eval ref (Write file) = comWrite ref file
eval ref Check        = comCheck ref
eval ref (Print n)    = comPrint ref n
eval ref (Sort n)     = comSort ref n
eval ref (Find word)  = comFind ref word

comRead :: IORef [Person] -> FilePath -> IO Result
comRead ref file = do
    csv <- readFile file
    case parseCSV csv of
        Left err -> return $ NG err
        Right es -> do
            let ps = map fromEntry es
                errors = lefts ps
                people = rights ps
                len = length people
            if errors == [] then do
                writeIORef ref people
                return $ OK ["read " ++ show len ++ " people"]
              else
                return $ NG (head errors)

-- FIXME: need to refactoring
comWrite :: IORef [Person] -> FilePath -> IO Result
comWrite ref file = do
    db <- readIORef ref
    let csv = map (entryToString.toEntry) db
        len = length csv
    withFile file WriteMode $ \hdl ->
        mapM_ (hPutStrLn hdl) csv
    return $ OK ["wrote " ++ show len ++ " people"]
  where
    entryToString = intercalate ","

comCheck :: IORef [Person] -> IO Result
comCheck ref = do
    len <- length <$> readIORef ref
    return $ OK [show len ++ " entries in DB"]

comPrint :: IORef [Person] -> Int -> IO Result
comPrint ref n = do
    db <- readIORef ref
    let db' | n == 0    = db
            | n >  0    = take n db
            | otherwise = takeEnd (negate n) db
    return $ OK (map show db')

comSort :: IORef [Person] -> Int -> IO Result
comSort ref n
  | n >= personEntryNumber = return $ NG $ show n ++ " is too large"
  | otherwise              = do
    modifyIORef ref $ sortBy (compareEntry n)
    return $ OK ["sorted"]

-- FIXME: is N 0-origin?
compareEntry :: Int-> Person -> Person -> Ordering
compareEntry 0 = compare `on` personId
compareEntry 1 = compare `on` personName
compareEntry 2 = compare `on` personBirthday
compareEntry 3 = compare `on` personAddress
compareEntry 4 = compare `on` personMisc
compareEntry _ = error "never reached"

comFind :: IORef [Person] -> String -> IO Result
comFind ref word = do
    db <- readIORef ref
    let db' = filter predicate db
    return $ OK $ map show db'
  where
    predicate psn = word `isInfixOf` personName psn
                 || word `isInfixOf` personAddress psn
                 || word `isInfixOf` personMisc psn
