module DB (DB, defaultDB, dbSize, dbEntries, newDB) where

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
