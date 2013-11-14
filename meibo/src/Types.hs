module Types where

data Command = Quit
             | Check
             | Print Int
             | Read FilePath
             | Write FilePath
             | Find String
             | Sort Int
             deriving (Eq,Show)

data Person = Person {
    personId       :: Int
  , personName     :: String
  , personBirthday :: (Int,Int,Int)
  , personAddress  :: String
  , personMisc     :: String
  } deriving (Eq,Show)

personEntryNumber :: Int
personEntryNumber = 5

data Result = OK [String] | NG String

type ParserError = String

data DB = DB {
    dbSize :: Int
  , dbEntries :: [Person]
  }

defaultDB :: DB
defaultDB = DB 0 []
