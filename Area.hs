module Area where
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
