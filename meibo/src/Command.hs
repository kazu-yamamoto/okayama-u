module Command (Command(..), command) where

import System.Exit (exitSuccess)

import CSVParser
import Person
import Types

command :: Command -> IO Result
command Quit = exitSuccess
command (Read file) = do
    csv <- readFile file
    case parseCSV csv of
        Left err -> print err
        Right es -> print (map fromCSV es)
    return OK
