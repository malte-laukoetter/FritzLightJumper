module Utilities where
import Network.MateLight.Simple
import TypeDefs

utiDiv :: Int -> Float -> Float
utiDiv a b = (fromIntegral a) / b

getScrollAlpha :: Time -> Int
getScrollAlpha time = floor $ utiDiv time 2

replaceNth :: Int -> a -> [a] -> [a]
-- PURPOSE
-- replaces the nth element of a list with the given value
replaceNth n val (x:xs)
     | n == 0    = val:xs
     | otherwise = x:replaceNth (n-1) val xs

replaceCanvasPos :: Pos -> Canvas -> Pixel -> Canvas
-- PURPOSE
-- Replaces the position of the canvas with the given pixel
replaceCanvasPos (x, y) canvas pixel =  replaceNth y (replaceNth x pixel (canvas!!y)) canvas

getCanvasPos :: Pos -> Canvas -> Pixel
-- PURPOSE
-- Gets the value of the position of the canvas
getCanvasPos (x, y) canvas = canvas!!y!!x

-- CONTRACT
digs :: Int -> [Int]

-- PURPOSE
-- Splits an Integer into its components

-- DEFINITION
digs 0 = []
digs x = x `mod` 10 : digs (floor $ utiDiv x 10)

-- CONTRACT
drawNumber :: [Int] -> Int -> Canvas -> Canvas

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

drawNumber []     index can = can
drawNumber (x:xs) index can = drawNumber xs (index+1) (helper 5 3 can)
     where getPixel r c   | ((numTemplate !! x) !! (r-1)) !! (c-1) = Pixel 255 255 255
                          | otherwise                              = Pixel 0 0 0
           helper 0 _ can = can
           helper r c can | c>0      = helper r (c-1) (replaceCanvasPos (c+index*4,r) can (getPixel r c))
                          | otherwise = helper (r-1) 3 (replaceCanvasPos (3+index*4,r) can (getPixel r 3))
