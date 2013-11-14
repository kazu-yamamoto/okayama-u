module DB (DB, defaultDB, dbSize, dbEntries, newDB, sortDB, findPeople) where

import Data.Function (on)
import Data.List (sortBy, isInfixOf)

import Types

data DB = DB Int [Person]

defaultDB :: DB
defaultDB = DB 0 []

newDB :: [Person] -> DB
newDB entries = DB (length entries) entries

dbSize :: DB -> Int
dbSize (DB size _) = size

dbEntries :: DB -> [Person]
dbEntries (DB _ entries) = entries

sortDB :: Int -> DB -> DB
sortDB n db = newDB (sortEntries entries)
  where
    entries = dbEntries db
    sortEntries = sortBy (compareEntry n)

-- FIXME: is N 0-origin?
compareEntry :: Int -> Person -> Person -> Ordering
compareEntry 0 = compare `on` personId
compareEntry 1 = compare `on` personName
compareEntry 2 = compare `on` personBirthday
compareEntry 3 = compare `on` personAddress
compareEntry 4 = compare `on` personMisc
compareEntry _ = error "never reached"

findPeople :: String -> DB -> [Person]
findPeople word db = filter predicate entries
  where
    entries = dbEntries db
    predicate entry =
        word `isInfixOf` personName entry
     || word `isInfixOf` personAddress entry
     || word `isInfixOf` personMisc entry
