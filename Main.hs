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


{-
- Utility functions
-}
utiDiv :: Int -> Float -> Float
utiDiv a b = (fromIntegral a) / b

inArea :: Area -> Pos -> Bool
inArea ((x1, y1), (x2, y2)) (x, y) = x>=x1 && x<=x2 && y>=y1 && y<=y2

inAreas :: [Area] -> Pos -> Bool
inAreas xs p = filter ((flip inArea) p) xs /= []

getScrollAlpha :: Time -> Int
getScrollAlpha time = floor $ utiDiv time 2

drawCanvas :: [Area] -> Time -> Canvas
drawCanvas areas time = [[if inAreas areas (x+aX, y) then Pixel 255 0 0 else Pixel 0 0 0 | x <- [0..(fst dim)-1]] | y <- [0..(snd dim)-1]]
  where aX = getScrollAlpha time

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
toFrame _ (a, b) = (ListFrame (drawCanvas vA (b+1)), (vA, b+1))
  where vA = genVisAreas (b+1)

-- We will probably build a whole new module out of this
-- 3 Elements visible at a time?
genVisAreas :: Time -> [Area]
genVisAreas time = bottom ++ [((xPos x, yPos x - size), (xPos x + size, yPos x)) | x <- [cX-2..cX+5]]
  where aX      = getScrollAlpha time
        cX      = floor $ utiDiv aX 10
        maxGap  = 2
        size    = 2
        gap x'  = (x' `mod` ((x' `mod` maxGap)+1))
        xPos x' = x'*10 + (gap x') + 5
        yPos x' | ((sin (utiDiv (2*x') 2.34))-0.5) > 0.5 = 10-size
                | ((sin (utiDiv (2*x') 2.34))-0.5) > 0   = 12-size
                | otherwise                              = 12
        bottom = [((0, 11), (150, 12))]
        --bottom = []
        --bottom = [((x', 11), (x',12)) | x' <- [aX..fst dim-1+aX], (sin $ utiDiv x' 10) > 0]

main :: IO ()
--main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "127.0.0.1") 1337 dim (Just 33000) False []) toFrame (level, 0)
main = Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 33000) False []) toFrame (genVisAreas 0, 0)
