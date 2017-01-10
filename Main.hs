module Main where
import Network.MateLight.Simple

import Data.Maybe
import qualified Network.Socket as Sock

dim :: (Int, Int)
dim = (30, 12)

type Pos = (Int, Int)
type Area = (Pos, Pos)
type Canvas = [[Pixel]]

inArea :: Area -> Pos -> Bool
inArea ((x1, y1), (x2, y2)) (x, y) = x>=x1 && x<=x2 && y>=y1 && y<=y2

inAreas :: [Area] -> Pos -> Bool
inAreas xs p = filter ((flip inArea) p) xs /= []

drawCanvas :: [Area] -> Canvas
drawCanvas areas = [[if inAreas areas (x, y) then Pixel 255 0 0 else Pixel 0 0 0 | x <- [0..(fst dim)-1]] | y <- [0..(snd dim)-1]]

toFrame :: [Event String] -> [Area] -> (ListFrame, [Area])
toFrame _ a = (ListFrame (drawCanvas a), a)

main :: IO ()
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 33000) False []) toFrame [((5,5), (10,10)), ((15,5), (20,10))]
