module Person where

import CSVParser
import Types
import Utils

fromCSV :: Entry -> Person
fromCSV [i,n,b,a,m] = Person (toInt i) n (toBirth b) a m

-- |
--
-- >>> toBirth "2011-1-13"
-- (2011,1,13)

toBirth :: String -> (Int,Int,Int)
toBirth b = (y,m,d)
  where
    [y,m,d] = map toInt . split '-' $ b
