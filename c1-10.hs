import Test

-- Problem 1
-- Find the last element of a list. 
c01 :: [a] -> a
c01 [] = error "Empty list"
c01 (x:[]) = x
c01 (x:xs) = c01 xs

test01 :: [Bool]
test01 = [ c01 [1,2,3,4] == 4
         , c01 ['x','y','z'] == 'z'
         ]

-- Problem 2
-- Find the last but one element of a list. 
c02 :: [a] -> a
c02 [] = error "Empty list"
c02 (x:[]) = error "Not enough elements"
c02 (x:_:[]) = x
c02 (x:xs) = c02 xs
         
test02 :: [Bool]
test02 = [ c02 [1,2,3,4] == 3
         , c02 ['a'..'z'] == 'y'
         ]

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1. 
c03 :: [a] -> Int -> a
c03 [] _ = error "Empty List"
c03 x i = x !! (i - 1)

test03 :: [Bool]
test03 = [ c03 [1,2,3] 2 == 2
         , c03 "haskell" 5 == 'e'
         ]

-- Problem 4
-- Find the number of elements of a list.
c04 :: [a] -> Int
c04 [] = 0
c04 (x:xs) = 1 + c04 xs

test04 :: [Bool]
test04 = [ c04 [123, 456, 789] == 3
         , c04 "Hello, world!" == 13
         ]

-- Problem 5
-- Reverse a list. 
c05 :: [a] -> [a]
c05 [] = []
c05 (x:xs) = (c05 xs) ++ [x]

test05 :: [Bool]
test05 = [ c05 "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A"
         , c05 [1,2,3,4] == [4,3,2,1]]

-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x). 

c06 :: Eq a => [a] -> Bool
c06 [] = True
c06 x = all id $ map (uncurry (==)) (zip x (c05 x))

test06 :: [Bool]
test06 = [ c06 [1,2,3] == False
         , c06 "madamimadam" == True
         , c06 [1,2,4,8,16,8,4,2,1] == True]

-- Problem 7
-- Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

c07 :: NestedList a -> [a]
c07 (Elem x) = [x]
c07 (List []) = []
c07 (List (x:xs)) = (c07 x) ++ (c07 (List xs))

test07 :: [Bool]
test07 = [ c07 (Elem 5) == [5]
         , c07 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) == [1,2,3,4,5]
         , (length $ c07 (List [])) == 0 -- Work around missing Eq a constraint for empty lists
         ]

-- Problem 8
-- Eliminate consecutive duplicates of list elements. 

c08 :: Eq a => [a] -> [a]
c08 [] = []
c08 (x:[]) = [x]
c08 (x:xs)
  | x == head (c08 xs) = c08 xs
  | otherwise = [x] ++ c08 xs

test08 :: [Bool]
test08 = [ c08 "aaaabccaadeeee" == "abcade"]

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists. 

splitC :: Eq a => [a] -> ([a], [a])
splitC [] = ([], [])
splitC (x:[]) = ([x], [])
splitC (x:xs) = (x : firstGroup, remainder)
  where
    firstGroup = takeWhile (== x) xs
    remainder = drop (length firstGroup) xs

iterate' :: ([a] -> ([a], [a])) -> ([[a]], [a]) -> ([[a]], [a])
iterate' fn (x, []) = (x, [])
iterate' fn (x, y)  = iterate' (fn) (x ++ [fst r], snd r)
  where
    r = fn y

c09 :: Eq a => [a] -> [[a]]
c09 [] = []
c09 x = fst groupedList
  where
    groupedList = iterate'
                  (splitC)
                  ([], x)


test09 :: [Bool]
test09 = [ c09 ['a', 'a', 'a', 'a', 'b', 'c', 'c',
                'a', 'a', 'd', 'e', 'e', 'e', 'e']
           == ["aaaa","b","cc","aa","d","eeee"]
         ]

-- Problem 10
-- Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding
-- data compression method. Consecutive duplicates of elements are encoded as
-- lists (N E) where N is the number of duplicates of the element E. 

c10 :: Eq a => [a] -> [(Int, a)]
c10 [] = []
c10 x = map (\x -> (length x, head x)) $ c09 x

test10 :: [Bool]
test10 = [ c10 "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
         ]

main = do
  putStr $ unlines $ map (runTest) [ test01
                                   , test02
                                   , test03
                                   , test04
                                   , test05
                                   , test06
                                   , test07
                                   , test08
                                   , test09
                                   , test10]
