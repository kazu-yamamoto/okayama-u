module Command where

data Command = Quit
             | Check
             | Print Int
             | Read FilePath
             | Write FilePath
             | Find String
             | Sort Int
