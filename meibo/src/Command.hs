module Command where

import System.Exit (exitSuccess)

data Command = Quit
             | Check
             | Print Int
             | Read FilePath
             | Write FilePath
             | Find String
             | Sort Int
             deriving (Eq,Show)

command :: Command -> IO ()
command Quit = exitSuccess
command _    = undefined
