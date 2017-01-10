module Main where
import Network.MateLight.Simple

import Data.Maybe
import qualified Network.Socket as Sock

dim :: (Int, Int)
dim = (30, 12)

type Time = Int

type Pos = (Int, Int)
type Area = (Pos, Pos)
type Canvas = [[Pixel]]

inArea :: Area -> Pos -> Bool
inArea ((x1, y1), (x2, y2)) (x, y) = x>=x1 && x<=x2 && y>=y1 && y<=y2

inAreas :: [Area] -> Pos -> Bool
inAreas xs p = filter ((flip inArea) p) xs /= []

drawCanvas :: [Area] -> Time -> Canvas
drawCanvas areas time = [[if inAreas areas (x+aX, y) then Pixel 255 0 0 else Pixel 0 0 0 | x <- [0..(fst dim)-1]] | y <- [0..(snd dim)-1]]
  where aX = floor (fromIntegral time/2);

-- Now we can basically build a small level
level :: [Area]
level = [
  ((0, 11), (10000000, 12)),
  ((10, 9), (13,12)),
  ((20, 9), (23,12)),
  ((30, 9), (33,12)),
  ((40, 9), (43,12)),
  ((50, 9), (53,12))]

toFrame :: [Event String] -> ([Area], Time) -> (ListFrame, ([Area], Time))
toFrame _ (a, b) = (ListFrame (drawCanvas a (b+1)), (a, b+1))

-- We will probably build a whole new module out of this
genVisAreas :: Time -> [Area]
genVisAreas time = undefined

main :: IO ()
--main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 33000) False []) toFrame (level, 0)
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 33000) False []) toFrame (level, 0)
