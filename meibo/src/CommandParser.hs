module CommandParser where

import Control.Applicative ((<*>), (<$), (<$>), (<*), (*>))
import Text.Parsec
import Text.Parsec.String (Parser)

import Command
import Types

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
-- >>> parseCommand "%X"
-- Left "unknown command"

parseCommand :: String -> Either ParserError Command
parseCommand xs = case parse pCommand "parseCommand" xs of
    Left _    -> Left "unknown command"
    Right cmd -> Right cmd

pCommand :: Parser Command
pCommand = char '%' *> pCmds <* ignoreWS

pCmds :: Parser Command
pCmds = choice [
    pQuit
  , pCheck
  , pPrint
  , pRead
  , pWrite
  , pFind
  , pSort
  ]

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
-- >>> parse num "num" "-35"
-- Right (-35)

num :: Parser Int
num = do
    func <- option id (negate <$ char '-')
    n <- read <$> many1 (oneOf ['0'..'9'])
    return $ func n

sp :: Parser ()
sp = () <$ many1 (char ' ')

ignoreWS :: Parser ()
ignoreWS = () <$ many (oneOf " \t\n")
