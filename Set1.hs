-- Ex 1: Find the last element of a list.

myLast :: [a] -> Maybe a
myLast []     = Nothing
myLast (x:[]) = Just x
myLast (_:xs) = myLast xs

-- Ex 2: Find the last but one element of a list.

myButLast :: [a] -> Maybe a
myButLast []       = Nothing
myButLast (x:_:_)  = Just x
myButLast (x:xs)   = myButLast xs

-- Ex 3: Find the K'th element of a list.
-- The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i-1)

-- Ex 4: Find the number of elements of a list.

myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

-- Ex 5: Reverse a list.

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
    