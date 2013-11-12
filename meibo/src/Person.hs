module Person where

import CSVParser
import Types
import Utils

fromCSV :: Entry -> Person
fromCSV [i,n,b,a,m] = Person (toInt i) n (toBirth b) a m
fromCSV err         = error $ "fromCSV: " ++ show err

-- |
--
-- >>> toBirth "2011-1-13"
-- (2011,1,13)

toBirth :: String -> (Int,Int,Int)
toBirth b = case map toInt (split '-' b) of
    [y,m,d] -> (y,m,d)
    _       -> error $ "toBirth: " ++ b
