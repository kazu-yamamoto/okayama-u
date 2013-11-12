module CommandParser where

import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<*>), (<$), (<$>))
import Data.Char

import Command

-- |
--
-- >>> parseCommand "%Q"
-- Right Quit
-- >>> parseCommand "%C"
-- Right Check
-- >>> parseCommand "%P 10"
-- Right (Print 10)
-- >>> parseCommand "%R test/sample.csv"
-- Right (Read "test/sample.csv")
-- >>> parseCommand "%W test/output.csv"
-- Right (Write "test/output.csv")
-- >>> parseCommand "%F keyword"
-- Right (Find "keyword")
-- >>> parseCommand "%S 2"
-- Right (Sort 2)

parseCommand :: String -> Either String Command
parseCommand xs = case parse command "parseCommand" xs of
    Left _    -> Left "error"
    Right cmd -> Right cmd

command :: Parser Command
command = do
    char '%'
    cmd <- pQuit <|> pCheck <|> pPrint <|> pRead <|> pWrite <|> pFind <|> pSort
    ignoreWS
    return cmd

pQuit :: Parser Command
pQuit = Quit <$ char 'Q'

pCheck :: Parser Command
pCheck = Check <$ char 'C'

pPrint :: Parser Command
pPrint = Print <$ (char 'P' >> sp) <*> num

pRead :: Parser Command
pRead  = Read  <$ (char 'R' >> sp) <*> pFile

pWrite :: Parser Command
pWrite = Write <$ (char 'W' >> sp) <*> pFile

pFind :: Parser Command
pFind  = Find  <$ (char 'F' >> sp) <*> pWord

pSort :: Parser Command
pSort  = Sort  <$ (char 'S' >> sp) <*> num

pFile :: Parser FilePath
pFile = many1 (noneOf " \t\n")

pWord :: Parser String
pWord = many1 (noneOf " \t\n")

-- |
--
-- >>> parse num "num" "10"
-- Right 10
-- >>> parse num "num" "2"
-- Right 2
-- >>> parse num "num" "35"
-- Right 35

num :: Parser Int
num = toInt <$> many1 (oneOf ['0'..'9'])
  where
    toInt = foldl (\x y -> x * 10 + y) 0 . map toI
    toI x = ord x - ord '0'

sp :: Parser ()
sp = () <$ many1 (char ' ')

ignoreWS :: Parser ()
ignoreWS = () <$ many (oneOf " \t\n")
