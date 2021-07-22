module Set2 where

import           Set1

-- Ex 11: Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element
-- has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.

data Encoded a = Multiple Int a | Single a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified xs = map encodeHelper (encode xs)

encodeHelper :: (Int, a) -> Encoded a
encodeHelper (x,y) = if x == 1
                     then Single y
                     else Multiple x y

-- Ex 12: Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.

decodeModified :: [Encoded a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = case x of (Single a) -> a:decodeModified xs
                                  (Multiple c a) -> repeat a ++ decodeModified xs

-- Ex 13: Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates,
-- as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

-- SEEN
encodeDirect :: (Eq a) => [a] -> [Encoded a]
encodeDirect xs = map encodeHelper (encode' xs)

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = foldr helper []
    where helper x [] = [(1,x)]
          helper x ((a,b):xs) = if x == b
                                then (1+a,x):xs
                                else (1,x):(a,b):xs

-- Ex 14: Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x:dupli xs

-- Ex 15: Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli [] _     = []
repli (x:xs) n = replicate n x ++ repli xs n
