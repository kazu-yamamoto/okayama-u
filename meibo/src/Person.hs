module Person where

import CSVParser
import Types
import Utils

fromEntry :: Entry -> Either String Person
fromEntry [i,n,b,a,m] = case parseBirth b of
    Just birth -> Right $ Person (read i) n birth a m
    Nothing    -> Left $ "parseBirth: " ++ b
fromEntry err   = Left $ "fromEntry: " ++ show err

toEntry :: Person -> Entry
toEntry (Person i n b a m) = [show i, n, ppBirth b, a, m]

-- |
--
-- >>> parseBirth "2011-1-13"
-- Just (2011,1,13)

parseBirth :: String -> Maybe (Int,Int,Int)
parseBirth b = case map read (split '-' b) of
    [y,m,d] -> Just (y,m,d)
    _       -> Nothing

ppBirth :: (Int,Int,Int) -> String
ppBirth (y,m,d) = show y ++ "-" ++ show m ++ "-" ++ show d
