module Utilities where
import Network.MateLight.Simple
import Data.Char
import TypeDefs

-- CONTRACT
utiDiv :: Int -> Float -> Float

-- PURPOSE
-- Divides an Integer by a float value and returns a float

-- DEFINITION
utiDiv a b = (fromIntegral a) / b

-- CONTRACT
getScrollAlpha :: Time -> Int

-- PURPOSE
-- Returns the scrolling offset depending on the time

-- DEFINITION
getScrollAlpha time = floor $ utiDiv time 2

-- CONTRACT
replaceNth :: Int -> a -> [a] -> [a]

-- PURPOSE
-- replaces the nth element of a list with the given value

-- DEFINITION
replaceNth n val (x:xs)
     | n == 0    = val:xs
     | otherwise = x:replaceNth (n-1) val xs

-- CONTRACT
replaceCanvasPos :: Pos -> Canvas -> Pixel -> Canvas

-- PURPOSE
-- Replaces the position of the canvas with the given pixel

-- DEFINITION
replaceCanvasPos (x, y) canvas pixel =  replaceNth y (replaceNth x pixel (canvas!!y)) canvas

-- CONTRACT
getCanvasPos :: Pos -> Canvas -> Pixel

-- PURPOSE
-- Gets the value of the position of the canvas

-- DEFINITION
getCanvasPos (x, y) canvas = canvas!!y!!x

-- CONTRACT
digs :: Int -> [Int]

-- PURPOSE
-- Splits an Integer into its components

-- DEFINITION
digs 0 = []
digs x = x `mod` 10 : digs (floor $ utiDiv x 10)

-- CONTRACT
drawNumber :: [Int] -> Int -> Pos -> Canvas -> Canvas

-- PURPOSE
-- Draws the time onto the canvas

-- DEFINITION
-- So each number has a template consisting out of 5x3 lists
numTemplate :: [[[Bool]]]
numTemplate = [
    [[False,True,False],[True,False,True],[True,False,True],[True,False,True],[False,True,False]], -- 0
    [[False,False,True],[False,True,True],[True,False,True],[False,False,True],[False,False,True]], -- 1
    [[False,True,True],[True,False,True],[False,True,False],[True,False,False],[True,True,True]], -- 2
    [[False,True,False],[False,False,True],[False,True,False],[False,False,True],[False,True,False]], -- 3
    [[True,False,False],[True,False,False],[True,True,True],[False,True,False],[False,True,False]], -- 4
    [[True,True,True],[True,False,False],[True,True,True],[False,False,True],[True,True,False]], -- 5
    [[True,True,True],[True,False,False],[True,True,True],[True,False,True],[True,True,True]], -- 6
    [[True,True,True],[False,False,True],[False,True,False],[False,True,False],[True,False,False]], -- 7
    [[False,True,False],[True,False,True],[False,True,False],[True,False,True],[False,True,False]], -- 8
    [[False,True,False],[True,False,True],[False,True,True],[False,False,True],[True,True,True]]] -- 9

drawNumber [] _ _ can                = can
drawNumber (x:xs) index (pX, pY) can = drawNumber xs (index+1) (pX, pY) (helper 5 3 can)
     where getPixel r c   | ((numTemplate !! x) !! (r-1)) !! (c-1) = Pixel 255 255 255
                          | otherwise                              = Pixel 0 0 0
           helper 0 _ can = can
           helper r c can | c>0       = helper r (c-1) (replaceCanvasPos (c+index*4-1+pX,r-1+pY) can (getPixel r c))
                          | otherwise = helper (r-1) 3 (replaceCanvasPos (3+index*4-1+pX,r-1+pY) can (getPixel r 3))

-- CONTRACT
drawText :: [Char] -> Int -> Pos -> Canvas -> Canvas

-- PURPOSE
-- Draws an uppercase english alphabetic text onto the canvas

-- DEFINITION
-- Again defining a template for each character
abcTemplate :: [[[Bool]]]
abcTemplate = [
    [[False,True,False],[True,False,True],[True,False,True],[True,True,True],[True,False,True]], -- A
    [[True,True,True],[True,False,True],[True,True,True],[True,False,True],[True,True,True]], -- B
    [[False,False,True],[False,True,False],[True,False,False],[False,True,False],[False,False,True]], -- C
    [[True,True,False],[True,False,True],[True,False,True],[True,False,True],[True,True,False]], -- D
    [[True,True,True],[True,False,False],[True,True,True],[True,False,False],[True,True,True]], -- E
    [[True,True,True],[True,False,False],[True,True,True],[True,False,False],[True,False,False]], -- F
    [[False,False,True],[False,True,False],[True,True,True],[True,False,True],[True,True,True]], -- G
    [[True,False,True],[True,False,True],[True,True,True],[True,False,True],[True,False,True]], -- H
    [[True,True,True],[False,True,False],[False,True,False],[False,True,False],[True,True,True]], -- I
    [[True,True,True],[False,False,True],[False,False,True],[False,True,False],[True,False,False]], -- J
    [[True,False,False],[True,False,True],[True,True,False],[True,False,True],[True,False,False]], -- K
    [[True,False,False],[True,False,False],[True,False,False],[True,False,False],[True,True,True]], -- L
    [[True,False,True],[True,True,True],[True,False,True],[True,False,True],[True,False,True]], -- M
    [[True,False,True],[True,True,True],[True,True,True],[True,True,True],[True,False,True]], -- N
    [[False,True,False],[True,False,True],[True,False,True],[True,False,True],[False,True,False]], -- O
    [[True,True,False],[True,False,True],[True,True,False],[True,False,False],[True,False,False]], -- P
    [[False,True,False],[True,False,True],[True,False,True],[False,True,False],[False,False,True]], -- Q
    [[True,True,False],[True,False,True],[True,False,True],[True,True,False],[True,False,True]], -- R
    [[False,True,False],[True,False,False],[False,True,False],[False,False,True],[False,True,False]], -- S
    [[True,True,True],[False,True,False],[False,True,False],[False,True,False],[False,True,False]], -- T
    [[True,False,True],[True,False,True],[True,False,True],[True,False,True],[False,True,False]], -- U
    [[True,False,True],[True,False,True],[True,False,True],[False,True,False],[False,True,False]], -- V
    [[True,False,True],[True,True,True],[True,False,True],[True,False,True],[True,False,True]], -- W
    [[True,False,True],[True,False,True],[False,True,False],[True,False,True],[True,False,True]], -- X
    [[True,False,True],[True,False,True],[False,True,False],[False,True,False],[False,True,False]], -- Y
    [[True,True,True],[False,False,True],[False,True,False],[True,False,False],[True,True,True]]] -- Z

-- using ord 'A' to convert - 65
drawText [] _ _ can                = can
drawText (x:xs) index (pX, pY) can = drawText xs (index+1) (pX, pY) (helper 5 3 can)
     where charNum        = (ord x) - 65
           getPixel r c   | ((abcTemplate !! charNum) !! (r-1)) !! (c-1) = Pixel 255 255 255
                          | otherwise                                    = Pixel 0 0 0
           helper 0 _ can = can
           helper r c can | c>0       = helper r (c-1) (replaceCanvasPos (c+index*4-1+pX,r-1+pY) can (getPixel r c))
                          | otherwise = helper (r-1) 3 (replaceCanvasPos (3+index*4-1+pX,r-1+pY) can (getPixel r 3))