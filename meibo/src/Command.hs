module Command (Command(..), command) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
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
                people = rights ps
                len = length people
            if errors == [] then do
                writeIORef ref (len,people)
                return $ OK ["read " ++ show len ++ " people"]
              else
                return $ NG (head errors)

comWrite :: IORef DB -> FilePath -> IO Result
comWrite ref file = do
    (_,db) <- readIORef ref
    let csv = map (entryToString.toEntry) db
        len = length csv
    withFile file WriteMode $ \hdl ->
        mapM_ (hPutStrLn hdl) csv
    return $ OK ["wrote " ++ show len ++ " people"]
  where
    entryToString = intercalate ","

comCheck :: IORef DB -> IO Result
comCheck ref = do
    (len,_) <- readIORef ref
    return $ OK [show len ++ " entries in DB"]

comPrint :: IORef DB -> Int -> IO Result
comPrint ref n = do
    db <- handleN . snd <$> readIORef ref
    return $ OK (map show db)
  where
    handleN db
      | n == 0    = db
      | n >  0    = take n db
      | otherwise = takeEnd (negate n) db

comSort :: IORef DB -> Int -> IO Result
comSort ref n
  | n >= personEntryNumber = return $ NG $ show n ++ " is too large"
  | otherwise              = do
    modifyIORef ref $ second (sortBy (compareEntry n))
    return $ OK ["sorted"]

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
    db <- findPeople word . snd <$> readIORef ref
    return $ OK $ map show db

findPeople :: String -> [Person] -> [Person]
findPeople word = filter predicate
  where
    predicate entry =
        word `isInfixOf` personName entry
     || word `isInfixOf` personAddress entry
     || word `isInfixOf` personMisc entry
