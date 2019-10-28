import Test
import Data.List (group)

-- Problem 11
-- Modified run-length encoding.
data RunLengthEnc a = Single a | Multiple Int a
  deriving (Eq, Show)

c11 :: Eq a => [a] -> [RunLengthEnc a]
c11 x = encodedList
  where
    encodedList = map (encode) (group x)
    encode y
      | length y == 1 = Single (head y)
      | otherwise = Multiple (length y) (head y)

test11 :: [Bool]
test11 = [c11 "aaaabccaadeeee" ==
          [Multiple 4 'a',Single 'b',Multiple 2 'c',
           Multiple 2 'a',Single 'd',Multiple 4 'e'] 
         ]

-- Problem 12
-- Decode a run-length encoded list.
c12 :: [RunLengthEnc a] -> [a]
c12 [] = []
c12 ((Single x):xs) = x : c12 xs
c12 ((Multiple i x):xs) = replicate i x ++ c12 xs

test12 :: [Bool]
test12 = [ c12 [Multiple 4 'a',Single 'b',Multiple 2 'c',
                Multiple 2 'a',Single 'd',Multiple 4 'e']
           == "aaaabccaadeeee"
         ]

-- Problem 13
-- Run-length encoding of a list (direct solution). 
c13 :: Eq a => [a] -> [RunLengthEnc a]
c13 [] = []
c13 (x:xs) = list
  where
    list = (fst lastStep) ++ [encodeOnce $ snd lastStep]
    lastStep = foldl (encode) ([], (1, x)) xs
    encodeOnce (n, e)
      | n == 1 = Single e
      | otherwise = Multiple n e
    encode (r, (n, l)) e
      | l == e = (r, (n + 1, e))
      | (l /= e) && (n == 1) = (r ++ [Single l], (1, e))
      | otherwise = (r ++ [Multiple n l], (1, e))

test13 :: [Bool]
test13 = [ c13 "aaaabccaadeeee"
           == [Multiple 4 'a',Single 'b',Multiple 2 'c',
               Multiple 2 'a',Single 'd',Multiple 4 'e']
         ]

-- Problem 14
-- Duplicate the elements of a list. 
c14 :: [a] -> [a]
c14 [] = []
c14 (x:xs) = [x, x] ++ c14 xs

test14 :: [Bool]
test14 = [ c14 [1, 2, 3] == [1,1,2,2,3,3]
         ]

-- Problem 15
-- Replicate the elements of a list a given number of times. 
c15 :: [a] -> Int -> [a]
c15 [] n = []
c15 (x:xs) n = (replicate n x) ++ c15 xs n

test15 :: [Bool]
test15 = [ c15 "abc" 3 == "aaabbbccc"
         ]

-- Problem 16
-- Drop every N'th element from a list. 
c16 :: [a] -> Int -> [a]
c16 [] _ = []
c16 x n = map (fst) $ filter (snd) $ zip x $ cycle $ (replicate (n-1) True) ++ [False]

test16 :: [Bool]
test16 = [ c16 "abcdefghik" 3 == "abdeghk"
         ]

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates. 
c17 :: [a] -> Int -> ([a], [a])
c17 [] _ = ([], [])
c17 x n = helper ([], x) n
  where
    helper (xs, ys) 0 = (xs, ys)
    helper (xs, y:ys) n = helper (xs ++ [y], ys) (n-1)

test17 :: [Bool]
test17 = [ c17 "abcdefghik" 3 == ("abc", "defghik")
         ]

-- Problem 18
-- Extract a slice from a list. 
c18 :: [a] -> Int -> Int -> [a]
c18 [] _ _ = []
c18 x i k = take (k - i + 1) $ drop (i - 1) x

test18 :: [Bool]
test18 = [ c18 ['a','b','c','d','e','f','g','h','i','k'] 3 7 == "cdefg"
         ]

-- Problem 19
-- Rotate a list N places to the left. 
c19 :: [a] -> Int -> [a]
c19 x n
  | n >= 0 = (drop n x) ++ (take n x)
  | otherwise = (drop (length x + n) x) ++ (take (length x + n) x)

test19 :: [Bool]
test19 = [ c19 ['a','b','c','d','e','f','g','h'] 3 == "defghabc"
         , c19 ['a','b','c','d','e','f','g','h'] (-2) == "ghabcdef"
         ]

-- Problem 20
-- Remove the K'th element from a list. 
c20 ::  Int -> [a] -> (a, [a])
c20 k x = (x !! (k - 1), take (k - 1) x ++ drop k x)

test20 :: [Bool]
test20 = [ c20 2 "abcd" == ('b',"acd")
         ]

main = do
  putStr $ unlines $ map (runTest) [ test11
                                   , test12
                                   , test13
                                   , test14
                                   , test15
                                   , test16
                                   , test17
                                   , test18
                                   , test19
                                   , test20]
