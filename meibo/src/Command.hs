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

command :: IORef DB -> Command -> IO Result
command ref cmd = eval ref cmd `catches` [Handler ehandler, Handler shandler]
  where
    ehandler :: ExitCode -> IO Result
    ehandler _ = exitSuccess
    shandler :: SomeException -> IO Result
    shandler e = return $ NG (show e)

eval :: IORef DB -> Command -> IO Result
eval _    Quit        = exitSuccess
eval ref (Read file)  = comRead ref file
eval ref (Write file) = comWrite ref file
eval ref Check        = comCheck ref
eval ref (Print n)    = comPrint ref n
eval ref (Sort n)     = comSort ref n
eval ref (Find word)  = comFind ref word

comRead :: IORef DB -> FilePath -> IO Result
comRead ref file = do
    csv <- readFile file
    case parseCSV csv of
        Left err -> return $ NG err
        Right es -> do
            let ps = map fromEntry es
                errors = lefts ps
                entries = rights ps
                size = length entries
            if errors == [] then do
                writeIORef ref $ DB size entries
                return $ OK ["read " ++ show size ++ " people"]
              else
                return $ NG (head errors)

comWrite :: IORef DB -> FilePath -> IO Result
comWrite ref file = do
    entries <- dbEntries <$> readIORef ref
    let csv = map (entryToString.toEntry) entries
        len = length csv
    withFile file WriteMode $ \hdl ->
        mapM_ (hPutStrLn hdl) csv
    return $ OK ["wrote " ++ show len ++ " people"]
  where
    entryToString = intercalate ","

comCheck :: IORef DB -> IO Result
comCheck ref = do
    size <- dbSize <$> readIORef ref
    return $ OK [show size ++ " entries in DB"]

comPrint :: IORef DB -> Int -> IO Result
comPrint ref n = do
    entries <- handleN . dbEntries <$> readIORef ref
    return $ OK (map show entries)
  where
    handleN entries
      | n == 0    = entries
      | n >  0    = take n entries
      | otherwise = takeEnd (negate n) entries

comSort :: IORef DB -> Int -> IO Result
comSort ref n
  | n >= personEntryNumber = return $ NG $ show n ++ " is too large"
  | otherwise              = do
    modifyIORef ref (sortDB n)
    return $ OK ["sorted"]

sortDB :: Int -> DB -> DB
sortDB n db = db {
    dbEntries = sortEntries entries
  }
  where
    entries = dbEntries db
    sortEntries = sortBy (compareEntry n)

-- FIXME: is N 0-origin?
compareEntry :: Int-> Person -> Person -> Ordering
compareEntry 0 = compare `on` personId
compareEntry 1 = compare `on` personName
compareEntry 2 = compare `on` personBirthday
compareEntry 3 = compare `on` personAddress
compareEntry 4 = compare `on` personMisc
compareEntry _ = error "never reached"

comFind :: IORef DB -> String -> IO Result
comFind ref word = do
    entries <- findPeople word <$> readIORef ref
    return $ OK $ map show entries

findPeople :: String -> DB -> [Person]
findPeople word db = filter predicate entries
  where
    entries = dbEntries db
    predicate entry =
        word `isInfixOf` personName entry
     || word `isInfixOf` personAddress entry
     || word `isInfixOf` personMisc entry
