module Main where
import Network.MateLight.Simple

import Data.Maybe
import qualified Network.Socket as Sock

type Pos = (Int, Int)
type Area = (Pos, Pos)

inArea :: Area -> Pos -> Bool
inArea ((x1, x2), (y1, y2)) (x, y) = x >= x1 && x <= x2 && y >= y1 && y <= y2

drawArea :: Area -> [[Pixel]]
drawArea a = map (\y -> map (\x -> if inArea a (x, y) then Pixel 0xff 0 0xff else Pixel 0 0 0) [0 .. (fst dim) - 1]) [0 .. (snd dim) - 1]

move :: (Int, Int) -> String -> (Int, Int) -> (Int, Int)
move (xdim, ydim) "\"j\"" (x, y) = (x, (y + 1) `mod` ydim)
move (xdim, ydim) "\"k\"" (x, y) = (x, (y - 1) `mod` ydim)
move (xdim, ydim) "\"h\"" (x, y) = ((x - 1) `mod` xdim, y)
move (xdim, ydim) "\"l\"" (x, y) = ((x + 1) `mod` xdim, y)
move _ _ x = x

toFrame :: (Int, Int) -> (Int, Int) -> ListFrame
toFrame (xdim, ydim) (x', y') = ListFrame (drawArea ((1, 5), (2, 10)))

eventTest :: [Event String] -> (Int, Int) -> (ListFrame, (Int, Int))
eventTest events pixel = (toFrame dim pixel', pixel')
  where pixel' = foldl (\acc (Event mod ev) -> if mod == "KEYBOARD" then move dim ev acc else acc) pixel events

dim :: (Int, Int)
dim = (30, 12)
main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 33000) False []) eventTest (0, 0)
