module Main where

import Example
import Recursion

-- import Lib
import Data.Char

lowerString :: [Char] -> [Char]
lowerString str = [ toLower loweredString | loweredString <- str]


average :: Float -> Float -> Float
average a b  = (a + b) / 2.0


averageList :: [Float] -> Float
averageList inputs = (sum inputs) / (fromIntegral (length inputs))

sayHello :: IO()
sayHello = do
    print ("What is your name?")
    value <- getLine
    print ("Hello " ++ value ++ "... You fucking cunt")

square :: Float -> Float
square x = x * x

-- pi :: Floating a => a
-- pi = 3.141592653589793

showAreaOfCircle :: Float -> IO ()
showAreaOfCircle radius = print ("The area of a circle with radius " ++ (show radius) ++ "cm is about " ++ (show (pi * (square radius))) ++ " cm^2")


maxi :: Ord a => a -> a -> a
maxi x y = if x >= y then x else y


-- signum :: (Ord a, Num a) => a -> Int
-- signum x = if x < 0 then -1 else if x == 0 then 0 else 1

-- GUARDS - conditional expressions if,then,else cascading
signumi :: (Ord a, Num a) => a -> Int
signumi x | x <  0  = -1
         | x == 0  = 0
         | x >  0  = 1


circleArea :: Floating a => a -> a
circleArea diameter  = pi * radius * radius
    where
        radius = diameter / 2.0       -- local binding


addMul :: Num a => a -> a -> (a, a)
addMul x y = (x + y, x * y)

-- Point definition from Cartesian Coordinate System
type Point = (Int, Int)


-- we represent colours by strings
--
type Colour = String

-- new name for the type of colour points
--
type ColourPoint = (Int, Int, Colour)

-- origin of the coordinate system in a given colour
--
origin :: Colour -> ColourPoint
origin colour  = (0, 0, colour)

-- move a colour point vertically and horizontally
--
move :: ColourPoint -> Int -> Int -> ColourPoint
move (x, y, colour) xDistance yDistance
  = (x + xDistance, y + yDistance, colour)

-- compute the distance between two colour points
--
-- fromIntegral :: (Integral a, Num b) => a -> b
distance :: ColourPoint -> ColourPoint -> Float
distance (x1, y1, colour1) (x2, y2, colour2)
  = sqrt (fromIntegral (dx * dx + dy * dy))
  where
    dx = x2 - x1
    dy = y2 - y1

oddNumbers :: Int -> [Int]
oddNumbers maxNumber  = [1, 3..maxNumber]

addToList :: a -> [a]
addToList value = value : []

-- Write a function sort2 :: Ord a => a -> a -> (a, a) which accepts two Int values as arguments and returns them as a sorted pair, so that sort2 5 3 is equal to (3,5). How can you define the function using a conditional, how can you do it using guards?
sort2 :: Ord a => a -> a -> (a, a)
sort2 x y | x > y = (x , y)
        | x < y = (y, x)
        | x == y = (x , y)

-- Define a function isLower :: Char -> Bool which returns True if a given character is a lower case letter. You can use the fact that characters are ordered, and for all lower case letters ch we have ′a′ ≤ ch and ch ≤ ′z′. Alternatively, you can use the fact that ['a'..'z'] evaluates to a list containing all lower case letters.
isLowercase :: Char -> Bool
isLowercase a = a == (toLower a)

-- Write a function mangle :: String -> String which removes the first letter of a word and attaches it at the end. If the string is empty, mangle should simply return an empty string
mangle :: String -> String
mangle value = (tail value) ++ [(head value)]


-- Implement division on Int, divide :: Int -> Int -> Int using the list functions described in this section. Hint: first, write a function that returns all the multiples of a given number up to a specific limit.
divide :: Int -> Int -> Int
divide a b = quot a b

main :: IO ()
main = do
    print ("Give me a number.")
    value <- getLine
    print ("The value is " ++ (show value))