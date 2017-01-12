module LevelGen where
import Network.MateLight.Simple
import TypeDefs
import Utilities

-- CONTRACT
inArea :: Area -> Pos -> Bool

-- PURPOSE
-- Returns True if given position is within an given area else false

-- DEFINITION
inArea ((x1, y1), (x2, y2)) (x, y) = x>=x1 && x<=x2 && y>=y1 && y<=y2

-- CONTRACT
inAreas :: [Area] -> Pos -> Bool

-- PURPOSE
-- Returns True if given position is within one of the given areas else false

-- DEFINITION
inAreas xs p = filter ((flip inArea) p) xs /= []

-- CONTRACT
drawAreasCanvas :: [Area] -> Time -> Canvas

-- CONTRACT
inAreasTime :: [Area] -> Time -> Pos -> Bool

-- PURPOSE
-- Is given positions within one of the given areas considering the scrolling due to time

-- DEFINITION
inAreasTime areas time (x,y) = inAreas areas (x+aX, y)
  where aX = getScrollAlpha time

-- PURPOSE
-- Generates an canvas including all generated areas

-- DEFINITION
drawAreasCanvas areas time = [[if inAreasTime areas time (x, y) then Pixel 100 100 100 else Pixel 0 0 0 | x <- [0..(fst dim)-1]] | y <- [0..(snd dim)-1]]

-- CONTRACT
genEmpty :: Canvas

-- PURPOSE
-- Generates an black canvas

-- DEFINITION
genEmpty = [[Pixel 0 0 0 | _ <- [0..29]] | _ <- [0..11]]

-- CONTRACT
drawCountDown :: Time -> Canvas

-- PURPOSE
-- Generates the count down canvas before the game actually starts

-- DEFINITION
drawCountDown time = drawNumber [nTime] 0 (13,3) genEmpty
  where nTime = -((floor $ utiDiv time 10) + 1)

-- CONTRACT
genVisAreas :: Time -> [Area]

-- PURPOSE
-- Generates a pseudo random area list where the player has to jump over

-- DEFINITION
genVisAreas time = [((xPos x, yPos x - size), (xPos x + size, yPos x)) | x <- [cX-2..cX+(5*maxBlo)]] ++ [bott x | x <- [cX-2..cX+(5*maxBlo)]]
  where aX      | time >= 30 = getScrollAlpha $ time - 30
                | otherwise  = 0
        cX      = floor $ utiDiv aX 10
        maxGap  | time < 2000 = 7
                | otherwise   = 3
        maxBlo  = (floor $ utiDiv time 2500) + 1 
        alpha   = floor $ utiDiv 10 (realToFrac maxBlo)
        size    = 2
        gap x'  | x' >= 5   = (x' `mod` ((x' `mod` maxGap)+1))
                | otherwise = 75 -- distance to first block
        xPos x' = x'*alpha + (gap x')
        yPos x' | ((tan (utiDiv (3*x') 7.23234))-0.4) > 0.2 = 9-size
                | ((tan (utiDiv (2*x') 3.34344))-0.5) > 0   = 12-size
                | otherwise                                 = 12
        bott x' | (sin $ utiDiv x' 10) > 0 || time < 40 || (tan $ utiDiv x' 24) > 0.1  = ((x'*alpha, 11), (x'*alpha+alpha, 12))
                | otherwise                                                            = ((0,0), (0,0))