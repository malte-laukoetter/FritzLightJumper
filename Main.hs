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

type Player = Pos

type GameState = ([Area], Time, Player)

inArea :: Area -> Pos -> Bool
inArea ((x1, y1), (x2, y2)) (x, y) = x>=x1 && x<=x2 && y>=y1 && y<=y2

inAreas :: [Area] -> Pos -> Bool
inAreas xs p = filter ((flip inArea) p) xs /= []

getScrollAlpha :: Time -> Int
getScrollAlpha time = floor (fromIntegral time/2)

drawAreasCanvas :: [Area] -> Time -> Canvas
drawAreasCanvas areas time = [[if inAreas areas (x+aX, y) then Pixel 255 255 0 else Pixel 0 0 0 | x <- [0..(fst dim)-1]] | y <- [0..(snd dim)-1]]
  where aX = getScrollAlpha time;

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

drawPlayerToCanvas :: Player -> Canvas -> Canvas
-- PURPOSE
-- draws the player to the canvas
drawPlayerToCanvas player canvas = replaceCanvasPos player canvas (Pixel 0 0 255)

drawCanvas :: GameState -> Canvas
-- PURPOSE
-- draws the canvas
drawCanvas (areas, time, player) = drawPlayerToCanvas player $ drawAreasCanvas areas time

increaseTime :: GameState -> GameState
-- PURPOSE
-- increases the current time of the gamestate by 1
increaseTime (a,t,b) = (a, t + 1, b)

-- Now we can basically build a small level
level :: [Area]
level = [
  ((10, 9), (13,12)),
  ((20, 9), (23,12)),
  ((30, 9), (33,12)),
  ((40, 9), (43,12)),
  ((50, 9), (53,12))]

toFrame :: [Event String] -> GameState -> (ListFrame, GameState)
toFrame _ (a, b, p) = (ListFrame (drawCanvas gameState), gameState)
  where
    vA        = genVisAreas (b+1)
    gameState = (vA, b+1, p)

-- We will probably build a whole new module out of this
-- 3 Elements visible at a time?
genVisAreas :: Time -> [Area]
genVisAreas time = [((aX, 11), (30+aX, 12))] ++ [((x*10+5, 9), (x*10+8, 12)) | x <- [cX-2..cX+5]]
  where aX = getScrollAlpha time
        cX = floor (fromIntegral aX/10)

main :: IO ()
--main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 33000) False []) toFrame (level, 0)
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 33000) False []) toFrame (genVisAreas 0, 0, (0, 8))
