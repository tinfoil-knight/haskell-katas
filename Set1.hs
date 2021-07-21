-- Ex 1: Find the last element of a list.

myLast :: [a] -> Maybe a
myLast []       = Nothing
myLast [x]      = Just x
myLast (_ : xs) = myLast xs

-- Ex 2: Find the last but one element of a list.

myButLast :: [a] -> Maybe a
myButLast []          = Nothing
myButLast (x : _ : _) = Just x
myButLast (x : xs)    = myButLast xs

-- Ex 3: Find the K'th element of a list.
-- The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt xs i = xs !! (i -1)

-- Ex 4: Find the number of elements of a list.

myLength :: [a] -> Int
myLength []       = 0
myLength (_ : xs) = 1 + myLength xs

-- Ex 5: Reverse a list.

myReverse :: [a] -> [a]
myReverse []       = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Ex 6: Find out whether a list is a palindrome.
-- A palindrome can be read forward or backward; e.g. (x a m a x).

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = True
isPalindrome [_] = True
isPalindrome xs  = (head xs == last xs) && isPalindrome ((tail . init) xs)

-- Ex 7: Flatten a nested list structure.

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List [])     = []
flatten (Elem x)      = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Ex 8: Eliminate consecutive duplicates of list elements.

compress :: (Eq a) => [a] -> [a]
compress []     = []
compress (x:xs) = x: dropWhile (==x) (compress xs)

-- Ex 9: Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.

pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x:f):pack s
    where f = takeWhile (==x) xs
          s = dropWhile (==x) xs

-- Ex 10: Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (pack xs)
