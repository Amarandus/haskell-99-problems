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

test07 :: [Bool]
test07 = []

test08 :: [Bool]
test08 = []

test09 :: [Bool]
test09 = []

test10 :: [Bool]
test10 = []

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
