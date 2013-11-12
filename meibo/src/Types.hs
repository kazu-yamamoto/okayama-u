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

data Result = OK | NG String
