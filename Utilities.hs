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