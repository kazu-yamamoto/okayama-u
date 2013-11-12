module Utils where

import Data.Char (ord)

-- |
--
-- >>> toInt "1"
-- 1
-- >>> toInt "23"
-- 23
-- >>> toInt "456"
-- 456
toInt :: String -> Int
toInt = foldl (\x y -> x * 10 + y) 0 . map toI
  where
    toI x = ord x - ord '0'

-- |
--
-- >>> split ',' "foo,bar,baz"
-- ["foo","bar","baz"]
-- >>> split ',' "foo,bar,"
-- ["foo","bar"]

split :: Char -> String -> [String]
split _ "" = []
split c s = case break (c==) s of
    ("",r)  -> split c (tail r)
    (s',"") -> [s']
    (s',r)  -> s' : split c (tail r)

-- |
--
-- >>> takeEnd 3 [0..4]
-- [2,3,4]
-- >>> takeEnd 5 [0..2]
-- [0,1,2]

takeEnd :: Int -> [a] -> [a]
takeEnd n xs = takeEnd' (drop n xs) xs
  where
    takeEnd' _      []     = []
    takeEnd' []     zs     = zs
    takeEnd' (_:ys) (_:zs) = takeEnd' ys zs
