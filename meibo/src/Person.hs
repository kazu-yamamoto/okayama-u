module Person where

import CSVParser
import Types
import Utils

fromCSV :: Entry -> Either String Person
fromCSV [i,n,b,a,m] = case toBirth b of
    Just birth -> Right $ Person (toInt i) n birth a m
    Nothing    -> Left $ "toBirth: " ++ b
fromCSV err     = Left $ "fromCSV: " ++ show err

-- |
--
-- >>> toBirth "2011-1-13"
-- Just (2011,1,13)

toBirth :: String -> Maybe (Int,Int,Int)
toBirth b = case map toInt (split '-' b) of
    [y,m,d] -> Just (y,m,d)
    _       -> Nothing
