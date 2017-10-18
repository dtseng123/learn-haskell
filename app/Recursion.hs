-- Lesson 3 Recursion
module Recursion where
import Data.Char


natSum :: (Num a, Ord a) => a -> a
natSum 0              = 0
natSum n  | n > 0     = n + natSum (n - 1)
          | otherwise = error "natSum: Input value too small!"

-- repeatN :: Int -> a -> [a]
-- repeatN 0 x  = []
-- repeatN n x  = x : repeatN (n - 1) x

suffixes :: String -> [String]
suffixes ""  = []
suffixes str = str : suffixes (tail str)

-- Apply a function to every element in a list - in this case square
allSquares :: Num a => [a] -> [a]
allSquares []       = []
allSquares (x : xs) = x * x : allSquares xs

allToUpper :: String -> String
allToUpper []                 = []
allToUpper (chr : restString) = toUpper chr : allToUpper restString


-- recursiveFunction []       = []
-- recursiveFunction (x : xs) = doSomethingWith x : recursiveFunction xs


extractDigits :: String -> String
extractDigits [] = []
extractDigits (chr : restString)
  | isDigit chr = chr : extractDigits restString
  | otherwise   =       extractDigits restString

-- multiply a list
multi :: Num a => [a] -> a
multi []     = 1
multi (x:xs) = x * multi xs

-- min of a list
minList :: [Int] -> Int
minList (x:[]) = x
minList (x:xs) = x `min` minList xs

